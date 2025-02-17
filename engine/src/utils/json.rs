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

pub(crate) struct JsonFile<T>(pub(crate) String, pub(crate) T);

impl<T> Deref for JsonFile<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl<T> DerefMut for JsonFile<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.1
    }
}

impl<T: Serialize> Serialize for JsonFile<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.1.serialize(serializer)
    }
}

impl<T: Serialize> Responder for JsonFile<T> {
    type Body = EitherBody<String>;

    fn respond_to(self, _req: &actix_web::HttpRequest) -> actix_web::HttpResponse<Self::Body> {
        match serde_json::to_string(&self.1) {
            Ok(body) => match HttpResponse::Ok()
                .content_type("application/json")
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
                HttpResponse::from_error(JsonPayloadError::Serialize(err)).map_into_right_body()
            }
        }
    }
}

impl<T> ApiComponent for JsonFile<T>
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
enum JsonPayloadError {
    #[error("serialization failed: {0}")]
    Serialize(serde_json::Error),
}

impl ResponseError for JsonPayloadError {
    fn status_code(&self) -> actix_web::http::StatusCode {
        actix_web::http::StatusCode::INTERNAL_SERVER_ERROR
    }
}
