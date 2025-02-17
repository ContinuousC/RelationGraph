/******************************************************************************
 * Copyright ContinuousC. Licensed under the "Elastic License 2.0".           *
 ******************************************************************************/

use actix_web::http::header::HeaderValue;

use crate::auth::AuthRole;

// Private zero-sized token.
#[derive(Clone, Copy, Debug)]
struct Token;

#[derive(Clone, Copy, Debug)]
pub(crate) struct AdminRole(Token);

#[derive(Clone, Copy, Debug)]
pub(crate) struct EditorRole(Token);

#[derive(Clone, Copy, Debug)]
pub(crate) struct ViewerRole(Token);

impl AuthRole for AdminRole {
    const ROLE_NAME: &'static str = "Admin";

    fn allowed(header: &HeaderValue) -> Option<Self> {
        matches!(header.as_bytes(), b"Admin").then(|| Self(Token))
    }
}

impl AuthRole for EditorRole {
    const ROLE_NAME: &'static str = "Editor";

    fn allowed(header: &HeaderValue) -> Option<Self> {
        matches!(header.as_bytes(), b"Admin" | b"Editor").then(|| Self(Token))
    }
}

impl From<AdminRole> for EditorRole {
    fn from(value: AdminRole) -> Self {
        Self(value.0)
    }
}

impl AuthRole for ViewerRole {
    const ROLE_NAME: &'static str = "Viewer";

    fn allowed(header: &HeaderValue) -> Option<Self> {
        matches!(header.as_bytes(), b"Admin" | b"Editor" | b"Viewer").then(|| Self(Token))
    }
}

impl From<AdminRole> for ViewerRole {
    fn from(value: AdminRole) -> Self {
        Self(value.0)
    }
}

impl From<EditorRole> for ViewerRole {
    fn from(value: EditorRole) -> Self {
        Self(value.0)
    }
}

impl EditorRole {
    /// Create a role token for an internal job. Do not use for
    /// user-initiated actions!
    pub(crate) const fn service() -> Self {
        Self(Token)
    }
}
