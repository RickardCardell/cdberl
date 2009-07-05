{ application, cdberl,
  [ 
    { description, "Mnesia interface for CouchDB." }, 
    { vsn, "0.1" },
    { modules, [ cdberl, cdberlsup, cdb_srv ] },
    { registered, [ cdberlsup, cdb_srv ] },
    { env, [ { default_server, {"localhost", 5984}} ] }, % { default_server, {IP/HOST:string(), PORT:int()}}
    { mod, { cdberl, [] } }    
  ] 
}.