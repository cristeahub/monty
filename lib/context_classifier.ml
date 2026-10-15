let command ?(model_config=`Null) harness command = match harness with
| Harness.Pi -> command ^ " --print --mode text --no-session --no-tools --no-extensions --no-skills --no-prompt-templates --no-context-files --no-themes --offline"
| Harness.Claude -> command ^ " --print --restricted --safe-mode --tools '' --strict-mcp-config --mcp-config '{\"mcpServers\":{}}' --setting-sources '' --settings '{\"disableAllHooks\":true}' --no-session-persistence"
| Harness.Codex ->
  let disabled=["shell_tool";"unified_exec";"apps";"plugins";"hooks";"multi_agent";"browser_use";"browser_use_external";"computer_use";"in_app_browser";"image_generation";"view_image";"code_mode";"code_mode_host";"skill_search";"skill_mcp_dependency_install";"memories";"goals";"tool_suggest";"in_app_local_automation";"workspace_dependencies";"default_mode_request_user_input"] in
  command ^ " exec --ignore-user-config --ignore-rules --ephemeral --skip-git-repo-check --sandbox read-only --color never"
    ^ Native_command.codex_model_arguments model_config
    ^ (List.map(fun name -> " -c " ^ Shell.quote("features." ^ name ^ "=false")) disabled |> String.concat "")
    ^ " -c 'approval_policy=\"never\"' -c 'web_search=\"disabled\"' -c 'mcp_servers={}' -c 'project_doc_max_bytes=0' -c 'skills.include_instructions=false' -c 'skills.bundled.enabled=false' -"
