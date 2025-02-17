/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Debug,
};

use chrono::{
    DateTime, Datelike, FixedOffset, LocalResult, Months, NaiveDate, NaiveDateTime, TimeZone,
    Timelike, Utc, Weekday,
};
use chrono_tz::Tz;
use dbschema::{DbTableId, FilterPath, ObjectId};
use serde::{de::DeserializeOwned, Deserialize, Serialize};

use relation_graph::{ItemId, RelationId};
use serde_json::json;

use crate::{roles::ViewerRole, AppData, Error, Result};

#[derive(Deserialize, Debug, schemars::JsonSchema, apistos::ApiComponent)]
pub struct BinsParams {
    #[serde(flatten)]
    events: EventParams,
    #[serde(flatten)]
    binning: BinningParams,
}

#[derive(Deserialize, Debug, schemars::JsonSchema, apistos::ApiComponent)]
pub struct EventParams {
    #[serde(flatten)]
    items: ItemsParams,
    #[serde(flatten)]
    date: TimeRange,
}

#[derive(Deserialize, schemars::JsonSchema)]
struct ItemsParams {
    items: BTreeSet<ItemId>,
    relations: BTreeSet<RelationId>,
}

impl Debug for ItemsParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ItemsParams")
            .field("items", &DebugLen(self.items.len(), "item(s)"))
            .field("relations", &DebugLen(self.relations.len(), "relation(s)"))
            .finish()
    }
}

struct DebugLen(usize, &'static str);

impl Debug for DebugLen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {})", self.0, self.1)
    }
}

#[derive(Deserialize, Debug, schemars::JsonSchema)]
struct TimeRange {
    from: DateTime<Utc>,
    to: DateTime<Utc>,
}

#[derive(Deserialize, Debug, schemars::JsonSchema)]
struct BinningParams {
    #[schemars(with = "String")]
    timezone: Tz,
    interval: Interval,
    #[serde(default = "one")]
    step: u32,
}

fn one() -> u32 {
    1
}

#[derive(Deserialize, Serialize, Debug, schemars::JsonSchema)]
#[serde(rename_all = "snake_case")]
enum Interval {
    Second,
    Minute,
    Hour,
    Day,
    Week,
    Month,
    Quarter,
    Year,
}

#[derive(Serialize, schemars::JsonSchema, Debug)]
pub struct Bin {
    from: DateTime<Utc>,
    to: DateTime<Utc>,
    count: u64,
}

#[derive(Deserialize)]
struct EsBinsRes {
    aggregations: EsAggr,
}

#[derive(Deserialize)]
struct EsAggr {
    events_over_time: EsBuckets,
}

#[derive(Deserialize)]
struct EsBuckets {
    buckets: Vec<EsBucket>,
}

#[derive(Deserialize)]
struct EsBucket {
    key_as_string: DateTime<FixedOffset>,
    doc_count: u64,
}

#[derive(Serialize, schemars::JsonSchema, Debug)]
pub struct Event<T> {
    pub timestamp: DateTime<Utc>,
    #[schemars(with = "String")]
    pub object_id: ObjectId,
    pub change_type: EventType,
    #[serde(flatten)]
    pub value: Option<T>,
}

#[derive(Serialize, schemars::JsonSchema, Debug)]
#[serde(rename_all = "snake_case")]
pub enum EventType {
    Added,
    Modified,
    Removed,
}

pub(crate) async fn run_bins_query(
    _role: ViewerRole,
    data: &AppData,
    params: BinsParams,
    table_id: &DbTableId,
) -> Result<Vec<Bin>> {
    let params = params.clamp_time_range()?;
    let res = data
        .es
        .post(
            data.es_url
                .join(format!("continuousc-{table_id}/_search").as_str())
                .map_err(Error::InvalidEsUrl)?,
        )
        .json(&json!({
            "query": params.events.esquery(),
            "aggs": {
                "events_over_time": {
                    "date_histogram": {
                        "field": "@active.from",
                        "calendar_interval": params.binning.interval,
                        "time_zone": params.binning.timezone,
                        "min_doc_count": 1
                    }
                }
            },
            "size": 0
        }))
        .send()
        .await
        .map_err(Error::ReqEvents)?
        .json::<serde_json::Value>()
        .await
        .map_err(Error::DecodeEvents)?;

    let res = serde_json::from_value::<EsBinsRes>(res).map_err(Error::DecodeESResponse)?;

    Ok(res
        .aggregations
        .events_over_time
        .buckets
        .iter()
        .try_fold(BTreeMap::new(), |mut bins, bucket| {
            let from = params
                .binning
                .floor(bucket.key_as_string.with_timezone(&Utc))
                .ok_or(Error::TimeHandling)?;
            let to = params.binning.next(from).ok_or(Error::TimeHandling)?;
            bins.entry(from).or_insert_with(|| Bin::new(from, to)).count += bucket.doc_count;
            Result::Ok(bins)
        })?
        .into_values()
        .collect())
}

#[derive(Serialize, schemars::JsonSchema, apistos::ApiComponent, Debug)]
pub struct BinnedEvents<T: schemars::JsonSchema> {
    events: Vec<Event<T>>,
    bins: Vec<Bin>,
}

pub(crate) async fn run_events_query<
    T: DeserializeOwned + schemars::JsonSchema,
    F: Fn(&Event<T>) -> bool,
>(
    _role: ViewerRole,
    data: &AppData,
    params: BinsParams,
    table_id: DbTableId,
    filter: F,
) -> Result<BinnedEvents<T>> {
    //let params = params.clamp_time_range()?;
    let events = data
        .db
        .query_discovery_objects_history(
            table_id,
            FilterPath::new()
                .field("item_id")
                .some()
                .eq_any(
                    params
                        .events
                        .items
                        .items
                        .iter()
                        .map(|id| json!(id))
                        .collect(),
                )
                .or(FilterPath::new().field("relation_id").some().eq_any(
                    params
                        .events
                        .items
                        .relations
                        .iter()
                        .map(|id| json!(id))
                        .collect(),
                )),
            dbschema::TimeRange::between(params.events.date.from, params.events.date.to),
        )
        .await
        .map_err(Error::Db)?;
    let mut events = events
        .into_iter()
        .flat_map(|(id, mut versions)| {
            // If the first object version returned by the query has
            // "from" within the query time range, it must have been
            // created, since otherwise we would find a previous
            // version with "to" set to the same timestamp. All
            // subsequent versions indicate modifications of the
            // object. If the last object version returned by the
            // query has "to" set inside the query time range, it must
            // have been "removed", since otherwise we would find a
            // next version active "from" the same timestamp. We
            // assume that objects cannot be "re-created" (with the
            // same object id) and that, consequently, there are no
            // gaps between versions of the same object.
            versions.sort_unstable_by(|a, b| a.version.active.from.cmp(&b.version.active.from));
            let removed = versions.last().and_then(|last| {
                last.version
                    .active
                    .to
                    .filter(|to| to < &params.events.date.to)
                    .map(|to| {
                        let value = serde_json::from_value::<T>(last.value.clone())
                            .map_err(|e| Error::DecodeEvent(id.clone(), e))?;
                        Ok(Event {
                            timestamp: to,
                            object_id: id.clone(),
                            change_type: EventType::Removed,
                            value: Some(value),
                        })
                    })
            });
            let last_index = versions.len() - 1;
            let mut versions = versions
                .into_iter()
                // Guard against "forgotten" objects in the DB. These
                // are returned either due to incomplete index
                // deletion or a bug in DbDaemon. This can be removed
                // when that is fixed.
                .enumerate()
                .filter(move |(i, v)| *i == last_index || v.version.active.to.is_some())
                .map(|(_, v)| v);
            let added = versions
                .next()
                .filter(|first| first.version.active.from >= params.events.date.from)
                .map(|first| {
                    let value = serde_json::from_value::<T>(first.value)
                        .map_err(|e| Error::DecodeEvent(id.clone(), e))?;
                    Ok(Event {
                        timestamp: first.version.active.from,
                        object_id: id.clone(),
                        change_type: EventType::Added,
                        value: Some(value),
                    })
                });
            added
                .into_iter()
                .chain(
                    versions
                        // Note: should not be necessary if input data is correct.
                        .filter(|first| first.version.active.from >= params.events.date.from)
                        .map(move |versioned| {
                            let value = serde_json::from_value::<T>(versioned.value)
                                .map_err(|e| Error::DecodeEvent(id.clone(), e))?;
                            Ok(Event {
                                timestamp: versioned.version.active.from,
                                object_id: id.clone(),
                                change_type: EventType::Modified,
                                value: Some(value),
                            })
                        }),
                )
                .chain(removed)
        })
        .filter(|r| r.as_ref().map_or(true, &filter))
        .collect::<Result<Vec<_>>>()?;
    events.sort_unstable_by(|a, b| {
        a.timestamp
            .cmp(&b.timestamp)
            .then_with(|| a.object_id.cmp(&b.object_id))
    });

    let bins = events
        .iter()
        .try_fold(BTreeMap::new(), |mut bins, event| {
            let from = params
                .binning
                .floor(event.timestamp)
                .ok_or(Error::TimeHandling)?;
            let to = params.binning.next(from).ok_or(Error::TimeHandling)?;
            bins.entry(from).or_insert_with(|| Bin::new(from, to)).count += 1;
            Result::Ok(bins)
        })?
        .into_values()
        .collect();

    Ok(BinnedEvents { events, bins })
}

impl BinsParams {
    fn clamp_time_range(self) -> Result<Self> {
        Ok(Self {
            events: EventParams {
                items: self.events.items,
                date: TimeRange {
                    from: self
                        .binning
                        .floor(self.events.date.from)
                        .ok_or(Error::TimeHandling)?,
                    to: self
                        .binning
                        .ceil(self.events.date.to)
                        .ok_or(Error::TimeHandling)?,
                },
            },
            binning: self.binning,
        })
    }
}

impl EventParams {
    fn esquery(&self) -> serde_json::Value {
        json!({
            "bool": {
                "must": [
                    self.items.esquery(),
                    self.date.es_query()
                ]
            }
        })
    }
}

impl ItemsParams {
    fn esquery(&self) -> serde_json::Value {
        json!({
            "bool": {
                "should": [
                    {
                        "terms": {
                            "item_id.keyword": self.items
                        }
                    },
                    {
                        "terms": {
                            "relation_id.keyword": self.relations
                        }
                    }
                ]
            }
        })
    }
}

impl TimeRange {
    fn es_query(&self) -> serde_json::Value {
        json!({
            "range": {
                "@active.from": {
                    "gte": self.from,
                    "lt": self.to
                }
            }
        })
    }
}

impl BinningParams {
    fn round(&self, t: DateTime<Utc>) -> Option<NaiveDateTime> {
        let local = t.with_timezone(&self.timezone);
        match self.interval {
            Interval::Second => local.date_naive().and_hms_opt(
                local.hour(),
                local.minute(),
                local.second().checked_div(self.step)? * self.step,
            ),
            Interval::Minute => local.date_naive().and_hms_opt(
                local.hour(),
                local.minute().checked_div(self.step)? * self.step,
                0,
            ),
            Interval::Hour => local.date_naive().and_hms_opt(
                local.hour().checked_div(self.step)? * self.step,
                0,
                0,
            ),
            Interval::Day => NaiveDate::from_ymd_opt(
                local.year(),
                local.month(),
                local.day().checked_div(self.step)? * self.step,
            )?
            .and_hms_opt(0, 0, 0),
            Interval::Week => NaiveDate::from_isoywd_opt(
                local.iso_week().year(),
                local.iso_week().week0().checked_div(self.step)? * self.step + 1,
                Weekday::Mon,
            )?
            .and_hms_opt(0, 0, 0),
            Interval::Month => NaiveDate::from_ymd_opt(
                local.year(),
                local.month0().checked_div(self.step)? * self.step + 1,
                1,
            )?
            .and_hms_opt(0, 0, 0),
            Interval::Quarter => {
                let step = 3 * self.step;
                NaiveDate::from_ymd_opt(local.year(), local.month().checked_div(step)? * step, 1)?
                    .and_hms_opt(0, 0, 0)
            }
            Interval::Year => NaiveDate::from_ymd_opt(
                local.year().checked_div(self.step as i32)? * self.step as i32,
                1,
                1,
            )?
            .and_hms_opt(0, 0, 0),
        }
    }

    fn floor(&self, t: DateTime<Utc>) -> Option<DateTime<Utc>> {
        Some(
            (0..=1)
                .filter_map(|n| {
                    // Note: this assumes the difference between
                    // summer and winter time is one hour (or a
                    // multiple thereof), which is probably not true
                    // for all timezones.
                    Some(self.timezone.from_local_datetime(
                        &self.round(t - chrono::Duration::try_hours(n).unwrap())?,
                    ))
                })
                .find_map(|r: LocalResult<DateTime<Tz>>| match r {
                    LocalResult::None => None,
                    LocalResult::Single(unambiguous) => Some(unambiguous),
                    LocalResult::Ambiguous(_, latest) if latest < t => Some(latest),
                    LocalResult::Ambiguous(earliest, _) => Some(earliest),
                })?
                .with_timezone(&Utc),
        )
    }

    fn ceil(&self, t: DateTime<Utc>) -> Option<DateTime<Utc>> {
        self.next(self.floor(t)?)
    }

    fn next(&self, t: DateTime<Utc>) -> Option<DateTime<Utc>> {
        // The documentation for chrono::Duration::try_seconds
        // specifies it returns `None` for values outside of the range
        // `-i64::MAX / 1_000` -- `i64::MAX / 1_000`. Since `step` is
        // u32m cast to i64 here, we can safely unwrap the result.
        let next = match self.interval {
            Interval::Second => t + chrono::Duration::try_seconds(self.step as i64).unwrap(),
            Interval::Minute => t + chrono::Duration::try_minutes(self.step as i64).unwrap(),
            Interval::Hour => t + chrono::Duration::try_hours(self.step as i64).unwrap(),
            Interval::Day => t + chrono::Duration::try_days(self.step as i64).unwrap(),
            Interval::Week => t + chrono::Duration::try_weeks(self.step as i64).unwrap(),
            Interval::Month => t.checked_add_months(Months::new(self.step))?,
            Interval::Quarter => t.checked_add_months(Months::new(self.step * 3))?,
            Interval::Year => t.checked_add_months(Months::new(self.step * 12))?,
        };
        Some(
            (0..=1)
                .filter_map(|n| {
                    Some(self.timezone.from_local_datetime(
                        &self.round(next + chrono::Duration::try_hours(n).unwrap())?,
                    ))
                })
                .find_map(|r: LocalResult<DateTime<Tz>>| match r {
                    LocalResult::None => None,
                    LocalResult::Single(unambiguous) => Some(unambiguous),
                    LocalResult::Ambiguous(earliest, _) if earliest > t => Some(earliest),
                    LocalResult::Ambiguous(_, latest) => Some(latest),
                })?
                .with_timezone(&Utc),
        )
    }
}

impl Bin {
    fn new(from: DateTime<Utc>, to: DateTime<Utc>) -> Self {
        Self { from, to, count: 0 }
    }
}

#[cfg(test)]
mod tests {
    use super::{BinningParams, Interval};
    use chrono::{DateTime, Utc};
    use chrono_tz::Tz;
    use std::str::FromStr;

    #[test]
    fn floor_1h() {
        let tz = Tz::from_str("Europe/Brussels").unwrap();
        let params = BinningParams {
            timezone: tz,
            interval: Interval::Hour,
            step: 1,
        };
        [
            ("2024-03-30T23:30:00Z", "2024-03-30T23:00:00Z"),
            ("2024-03-31T00:30:00Z", "2024-03-31T00:00:00Z"),
            ("2024-10-27T00:30:00Z", "2024-10-27T00:00:00Z"),
            ("2024-10-27T01:30:00Z", "2024-10-27T01:00:00Z"),
        ]
        .into_iter()
        .for_each(|(t, expected)| {
            let t = DateTime::<Utc>::from_str(t).unwrap();
            assert_eq!(
                params
                    .floor(t)
                    .unwrap()
                    .to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
                expected
            )
        });
    }

    #[test]
    fn winter_to_summer_1h() {
        let tz = Tz::from_str("Europe/Brussels").unwrap();
        let params = BinningParams {
            timezone: tz,
            interval: Interval::Hour,
            step: 1,
        };
        let t = DateTime::<Utc>::from_str("2024-03-30T23:30:00Z").unwrap();
        let bins = std::iter::successors(Some(params.floor(t).unwrap()), |t| {
            Some(params.next(*t).unwrap())
        });
        assert_eq!(
            bins.take(4)
                .map(|t| format!(
                    "{} - {}",
                    t.to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
                    t.with_timezone(&tz)
                ))
                .collect::<Vec<_>>(),
            [
                "2024-03-30T23:00:00Z - 2024-03-31 00:00:00 CET",
                "2024-03-31T00:00:00Z - 2024-03-31 01:00:00 CET",
                "2024-03-31T01:00:00Z - 2024-03-31 03:00:00 CEST",
                "2024-03-31T02:00:00Z - 2024-03-31 04:00:00 CEST"
            ]
        );
    }

    #[test]
    fn summer_to_winter_1h() {
        let tz = Tz::from_str("Europe/Brussels").unwrap();
        let params = BinningParams {
            timezone: tz,
            interval: Interval::Hour,
            step: 1,
        };
        let t = DateTime::<Utc>::from_str("2024-10-27T00:30:00Z").unwrap();
        let bins = std::iter::successors(Some(params.floor(t).unwrap()), |t| {
            Some(params.next(*t).unwrap())
        });
        assert_eq!(
            bins.take(4)
                .map(|t| format!(
                    "{} - {}",
                    t.to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
                    t.with_timezone(&tz)
                ))
                .collect::<Vec<_>>(),
            [
                "2024-10-27T00:00:00Z - 2024-10-27 02:00:00 CEST",
                "2024-10-27T01:00:00Z - 2024-10-27 02:00:00 CET",
                "2024-10-27T02:00:00Z - 2024-10-27 03:00:00 CET",
                "2024-10-27T03:00:00Z - 2024-10-27 04:00:00 CET"
            ]
        );
    }

    #[test]
    fn winter_to_summer_2h() {
        let tz = Tz::from_str("Europe/Brussels").unwrap();
        let params = BinningParams {
            timezone: tz,
            interval: Interval::Hour,
            step: 2,
        };
        let t = DateTime::<Utc>::from_str("2024-03-30T23:30:00Z").unwrap();
        let bins = std::iter::successors(Some(params.floor(t).unwrap()), |t| {
            Some(params.next(*t).unwrap())
        });
        assert_eq!(
            bins.take(4)
                .map(|t| format!(
                    "{} - {}",
                    t.to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
                    t.with_timezone(&tz)
                ))
                .collect::<Vec<_>>(),
            [
                "2024-03-30T23:00:00Z - 2024-03-31 00:00:00 CET",
                "2024-03-31T02:00:00Z - 2024-03-31 04:00:00 CEST",
                "2024-03-31T04:00:00Z - 2024-03-31 06:00:00 CEST",
                "2024-03-31T06:00:00Z - 2024-03-31 08:00:00 CEST"
            ]
        );
    }

    #[test]
    fn summer_to_winter_2h() {
        let tz = Tz::from_str("Europe/Brussels").unwrap();
        let params = BinningParams {
            timezone: tz,
            interval: Interval::Hour,
            step: 2,
        };
        let t = DateTime::<Utc>::from_str("2024-10-27T00:30:00Z").unwrap();
        let bins = std::iter::successors(Some(params.floor(t).unwrap()), |t| {
            Some(params.next(*t).unwrap())
        });
        assert_eq!(
            bins.take(4)
                .map(|t| format!(
                    "{} - {}",
                    t.to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
                    t.with_timezone(&tz)
                ))
                .collect::<Vec<_>>(),
            [
                "2024-10-27T00:00:00Z - 2024-10-27 02:00:00 CEST",
                "2024-10-27T01:00:00Z - 2024-10-27 02:00:00 CET",
                "2024-10-27T03:00:00Z - 2024-10-27 04:00:00 CET",
                "2024-10-27T05:00:00Z - 2024-10-27 06:00:00 CET"
            ]
        );
    }

    #[test]
    fn winter_to_summer_5m() {
        let tz = Tz::from_str("Europe/Brussels").unwrap();
        let params = BinningParams {
            timezone: tz,
            interval: Interval::Minute,
            step: 5,
        };
        let t = DateTime::<Utc>::from_str("2024-03-31T00:53:21Z").unwrap();
        let bins = std::iter::successors(Some(params.floor(t).unwrap()), |t| {
            Some(params.next(*t).unwrap())
        });
        assert_eq!(
            bins.take(4)
                .map(|t| format!(
                    "{} - {}",
                    t.to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
                    t.with_timezone(&tz)
                ))
                .collect::<Vec<_>>(),
            [
                "2024-03-31T00:50:00Z - 2024-03-31 01:50:00 CET",
                "2024-03-31T00:55:00Z - 2024-03-31 01:55:00 CET",
                "2024-03-31T01:00:00Z - 2024-03-31 03:00:00 CEST",
                "2024-03-31T01:05:00Z - 2024-03-31 03:05:00 CEST"
            ]
        );
    }

    #[test]
    fn summer_to_winter_5m() {
        let tz = Tz::from_str("Europe/Brussels").unwrap();
        let params = BinningParams {
            timezone: tz,
            interval: Interval::Minute,
            step: 5,
        };
        let t = DateTime::<Utc>::from_str("2024-10-27T00:53:21Z").unwrap();
        let bins = std::iter::successors(Some(params.floor(t).unwrap()), |t| {
            Some(params.next(*t).unwrap())
        });
        assert_eq!(
            bins.take(4)
                .map(|t| format!(
                    "{} - {}",
                    t.to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
                    t.with_timezone(&tz)
                ))
                .collect::<Vec<_>>(),
            [
                "2024-10-27T00:50:00Z - 2024-10-27 02:50:00 CEST",
                "2024-10-27T00:55:00Z - 2024-10-27 02:55:00 CEST",
                "2024-10-27T01:00:00Z - 2024-10-27 02:00:00 CET",
                "2024-10-27T01:05:00Z - 2024-10-27 02:05:00 CET"
            ]
        );
    }
}
