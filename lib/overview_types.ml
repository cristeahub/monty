type github_source = {
  repo : string;
  query : string option;
}

type source = Github_issues of github_source

type raw_project = {
  persisted_id : string option;
  repo : string;
  sources : source list;
}

type project = {
  id : string;
  repo : string;
  sources : source list;
}

type local_task = {
  id : string;
  project : string;
  title : string;
  status : string;
  branch : string option;
  notes : string option;
  worker_id : string option;
  worker_key : string option;
  external_key : string option;
  external_url : string option;
  external_source : string option;
  created_at : string option;
  updated_at : string option;
}

type task = {
  key : string;
  display_id : string;
  project : string;
  origin : string;
  title : string;
  status : string;
  branch : string option;
  url : string option;
}

type sync_result = {
  created : int;
  updated : int;
  linked_jobs : int;
  warnings : string list;
}
