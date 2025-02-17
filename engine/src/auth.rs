/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use std::future::{ready, Ready};

use actix_web::{http::header::HeaderValue, FromRequest, ResponseError};
use apistos::ApiComponent;

#[derive(Clone, Copy, Debug)]
pub(crate) struct Auth<T>(T);

impl<T> Auth<T> {
    pub(crate) fn into_inner(self) -> T {
        self.0
    }
}

pub(crate) trait AuthRole: Sized {
    const ROLE_NAME: &'static str;
    /// Check if X-PROXY-ROLE value includes this role.  This should
    /// be the only way to construct the role token, so that having a
    /// token can be used as evidence that the user is authorized to
    /// run a command.
    fn allowed(header: &HeaderValue) -> Option<Self>;
}

impl<T: AuthRole> FromRequest for Auth<T> {
    type Error = AuthError;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _payload: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        if let Some(token) = req.headers().get("X-PROXY-ROLE").and_then(T::allowed) {
            ready(Ok(Self(token)))
        } else {
            ready(Err(AuthError::Unauthorized))
        }
    }
}

impl<T: AuthRole> ApiComponent for Auth<T> {
    fn child_schemas() -> Vec<(String, apistos::reference_or::ReferenceOr<apistos::Schema>)> {
        Vec::new()
    }

    fn schema() -> Option<(String, apistos::reference_or::ReferenceOr<apistos::Schema>)> {
        None
    }

    fn security_requirement_name() -> Option<String> {
        Some(String::from(T::ROLE_NAME))
    }
}

#[derive(thiserror::Error, Debug)]
pub(crate) enum AuthError {
    #[error("unauthorized")]
    Unauthorized,
}

impl ResponseError for AuthError {
    fn status_code(&self) -> actix_web::http::StatusCode {
        actix_web::http::StatusCode::FORBIDDEN
    }
}
