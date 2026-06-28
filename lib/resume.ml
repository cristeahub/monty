let find_record ~home ?(scope = Job_store.Active) needle =
  Job_store.find ~home ~scope needle

let find ~home ?(scope = Job_store.Active) needle =
  find_record ~home ~scope needle |> Result.map (fun record -> record.Job_store.job)
