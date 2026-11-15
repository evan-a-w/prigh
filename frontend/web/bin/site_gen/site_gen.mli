(** [site_gen sw TEMPLATE FILE...] prints [TEMPLATE] with [@@VERSION@@]
    replaced by a hash of the files' names and contents and [@@SHELL@@] by
    their names as a JSON array, each as the page asks for it:
    [NAME?v=VERSION] where one of the files names it so, else its base name.

    [site_gen versioned TEMPLATE FILE...] prints [TEMPLATE] with each quoted
    base name of the files (["main.bc.js"] or ['main.bc.js']) as
    [NAME?v=VERSION].

    A file's version is the first 16 hex digits of the MD5 of its contents:
    the backend serves [NAME?v=VERSION] as immutable when it is the file's
    ([Static_site]). *)
