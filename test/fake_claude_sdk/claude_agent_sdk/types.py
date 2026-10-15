from dataclasses import dataclass
@dataclass
class PermissionResultAllow:
    updated_input: dict
@dataclass
class PermissionResultDeny:
    message: str
