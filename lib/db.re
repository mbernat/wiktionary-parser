open Sqlite3

let create_table_sql = "CREATE TABLE contacts ( \
                          contact_id INTEGER PRIMARY KEY, \
                          first_name TEXT NOT NULL, \
                          last_name TEXT NOT NULL, \
                          email text NOT NULL UNIQUE, \
                          phone text NOT NULL UNIQUE \
                        );";

let play = () => {
    let db = db_open("dict.db");
    let () =
        switch (exec(db, create_table_sql)) {
        | Rc.OK => print_endline("Ok")
        | r =>
            prerr_endline(Rc.to_string(r));
            prerr_endline(errmsg(db));
        };
}