{ application, counter_service, [
	{ description, "Simple counter sevice for erlang applications"},
	{ vsn, "0.1"},
	{ modules, [
		counter_service,
		cs_app,
		cs_domain,
		cs_domain_sup,
		cs_filestore_csv,
		cs_filestore,
		cs_filestore_json,
		cs_filestore_xml,
		cs_lock,
		cs_lock_sup,
		cs_scheduler,
		cs_scheduler_task,
		cs_scheduler_task_sup,
		cs_scheduler_timer,
		cs_service,
		cs_sup,
		cs_util
	] },
	{ registered, [] },
	{ applications, [] },
	{ mod, { cs_app, [] }}
]}.
