/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::ops::{Deref, DerefMut};

use actix_web::{
    body::EitherBody,
    http::{header::ACCEPT, StatusCode},
    HttpResponse, Responder, ResponseError,
};
use apistos::{reference_or::ReferenceOr, ApiComponent, Schema};
use serde::Serialize;

/// Bitcode- or JSON-serialized response depending on the accept
/// header.
pub(crate) struct Bitcode<T>(pub(crate) T);

impl<T> Deref for Bitcode<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Bitcode<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: Serialize> Responder for Bitcode<T> {
    type Body = EitherBody<Vec<u8>>;

    fn respond_to(self, req: &actix_web::HttpRequest) -> actix_web::HttpResponse<Self::Body> {
        let Some(accept) = req.headers().get(ACCEPT) else {
            return HttpResponse::from_error(BitcodeError::MissingAcceptHeader)
                .map_into_right_body();
        };
        let r = accept
            .as_bytes()
            .split(|b| *b == b',')
            .find_map(|ty| match ty.trim_ascii() {
                b"application/json" => Some(match serde_json::to_vec(&self.0) {
                    Ok(body) => Ok(HttpResponse::Ok()
                        .content_type("application/json")
                        .message_body(body)),
                    Err(e) => Err(HttpResponse::from_error(BitcodeError::SerializeJson(e))),
                }),
                b"application/x-bitcode" => Some(Ok(HttpResponse::Ok()
                    .content_type("application/x-bitcode")
                    .message_body(bitcode::serialize(&self.0).unwrap()))),
                b"application/x-bincode" => Some(match bincode::serialize(&self.0) {
                    Ok(body) => Ok(HttpResponse::Ok()
                        .content_type("application/x-bincode")
                        .message_body(body)),
                    Err(e) => Err(HttpResponse::from_error(BitcodeError::SerializeBincode(e))),
                }),
                _ => None,
            });
        match r {
            Some(Ok(Ok(res))) => res.map_into_left_body(),
            Some(Ok(Err(e))) => HttpResponse::from_error(e).map_into_right_body(),
            Some(Err(e)) => e.map_into_right_body(),
            None => HttpResponse::from_error(BitcodeError::UnacceptableAcceptHeader)
                .map_into_right_body(),
        }
    }
}

impl<T> ApiComponent for Bitcode<T>
where
    T: ApiComponent,
{
    fn required() -> bool {
        T::required()
    }

    fn child_schemas() -> Vec<(String, ReferenceOr<Schema>)> {
        T::child_schemas()
    }

    fn raw_schema() -> Option<ReferenceOr<Schema>> {
        T::raw_schema()
    }

    fn schema() -> Option<(String, ReferenceOr<Schema>)> {
        T::schema()
    }
}

#[derive(thiserror::Error, Debug)]
enum BitcodeError {
    #[error("missing 'Accept' header")]
    MissingAcceptHeader,
    #[error("unacceptable 'Accept' header")]
    UnacceptableAcceptHeader,
    #[error("json serialization failed: {0}")]
    SerializeJson(serde_json::Error),
    #[error("bincode serialization failed: {0}")]
    SerializeBincode(bincode::Error),
}

impl ResponseError for BitcodeError {
    fn status_code(&self) -> actix_web::http::StatusCode {
        match self {
            BitcodeError::MissingAcceptHeader | BitcodeError::UnacceptableAcceptHeader => {
                StatusCode::NOT_ACCEPTABLE
            }
            BitcodeError::SerializeJson(_) | BitcodeError::SerializeBincode(_) => {
                StatusCode::INTERNAL_SERVER_ERROR
            }
        }
    }
}
