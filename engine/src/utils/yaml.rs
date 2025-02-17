/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::ops::{Deref, DerefMut};

use actix_web::{
    body::EitherBody,
    http::header::{ContentDisposition, DispositionParam, DispositionType},
    HttpResponse, Responder, ResponseError,
};
use apistos::{reference_or::ReferenceOr, ApiComponent, Schema};
use serde::Serialize;

pub(crate) struct YamlFile<T>(pub(crate) String, pub(crate) T);

impl<T> Deref for YamlFile<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl<T> DerefMut for YamlFile<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.1
    }
}

impl<T: Serialize> Serialize for YamlFile<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.1.serialize(serializer)
    }
}

impl<T: Serialize> Responder for YamlFile<T> {
    type Body = EitherBody<String>;

    fn respond_to(self, _req: &actix_web::HttpRequest) -> actix_web::HttpResponse<Self::Body> {
        match serde_yaml::to_string(&self.1) {
            Ok(body) => match HttpResponse::Ok()
                .content_type("application/yaml")
                .append_header(ContentDisposition {
                    disposition: DispositionType::Attachment,
                    parameters: Vec::from_iter([DispositionParam::Filename(self.0)]),
                })
                .message_body(body)
            {
                Ok(res) => res.map_into_left_body(),
                Err(err) => HttpResponse::from_error(err).map_into_right_body(),
            },

            Err(err) => {
                HttpResponse::from_error(YamlPayloadError::Serialize(err)).map_into_right_body()
            }
        }
    }
}

impl<T> ApiComponent for YamlFile<T>
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
enum YamlPayloadError {
    #[error("serialization failed: {0}")]
    Serialize(serde_yaml::Error),
}

impl ResponseError for YamlPayloadError {
    fn status_code(&self) -> actix_web::http::StatusCode {
        actix_web::http::StatusCode::INTERNAL_SERVER_ERROR
    }
}
