(define-module (reformer db psql)
  #:export (db/apply-ddl
            db/close
            db/open
			      db/query
            db/test
            db/with
            db/connection-string
			      db/test-connection-string
			      db/ddl-file-path
            exec-status-type
            exec-status-type-values
            exec-status-type-set
            int->ExecStatusType
            int->enum))

(use-modules (f)
             (reformer config)
             (reformer models)
             (ice-9 format)
             (system foreign)
             ((rnrs) #:prefix rnrs:)
             (rnrs bytevectors)
             (rnrs enums))

(define db/connection-string (getenv "REFORMER_CONNECTION_STRING"))
(define db/test-connection-string (getenv "REFORMER_TEST_CONNECTION_STRING"))
(define db/ddl-file-path (or (getenv "REFORMER_DB_DDL_FILE_PATH") "src/ddl.psql.sql"))
(define libpq (dynamic-link (getenv "PSQL_LIBRARY_PATH")))

(define (int->enum enum-set n)
  (if (>= n (length exec-status-type-values))
      #f
      (list-ref exec-status-type-values n)))

(define-enumeration
  exec-status-type
  (PGRES_EMPTY_QUERY      ; empty query string was executed
   PGRES_COMMAND_OK       ; a query command that doesn't return anything was executed properly by the backend
   PGRES_TUPLES_OK        ; a query command that returns tuples was executed properly by the backend, PGresult contains the result tuples
   PGRES_COPY_OUT         ; Copy Out data transfer in progress
   PGRES_COPY_IN          ; Copy In data transfer in progress
   PGRES_BAD_RESPONSE     ; an unexpected response was recv'd from the backend
   PGRES_NONFATAL_ERROR   ; notice or warning message
   PGRES_FATAL_ERROR      ; query failed
   PGRES_COPY_BOTH        ; Copy In/Out data transfer in progress
   PGRES_SINGLE_TUPLE     ; single tuple from larger resultset
   PGRES_PIPELINE_SYNC    ; pipeline synchronization point
   PGRES_PIPELINE_ABORTED ; Command didn't run because of an abort earlier in a pipeline
   PGRES_TUPLES_CHUNK     ; chunk of tuples from larger resultset
   )
  exec-status-type-set)

(define exec-status-type-values
  (enum-set->list (enum-set-universe (exec-status-type-set))))

(define (int->exec-status-type status)
  "Source: https://doxygen.postgresql.org/libpq-fe_8h.html#a16a9effd60c2048ab36ab3bf52bede2c"
  (int->enum (exec-status-type-set) status))

(define *PQconnectdb*
  (pointer->procedure
   '*
   (dynamic-func "PQconnectdb" libpq)
   (list '*)))

(define *PQexec*
  (pointer->procedure
   '*
   (dynamic-func "PQexec" libpq)
   (list '* '*)))

(define *PQfinish*
  (pointer->procedure
   void
   (dynamic-func "PQfinish" libpq)
   (list '*)))

(define *PQresultStatus*
  (pointer->procedure
   int
   (dynamic-func "PQresultStatus" libpq)
   (list '*)))

(define (PQresultStatus result)
  (let ((status (*PQresultStatus* result)))
    (int->exec-status-type status)))

(define *PQntuples*
  (pointer->procedure
   int
   (dynamic-func "PQntuples" libpq)
   (list '*)))

(define *PQnfields*
  (pointer->procedure
   int
   (dynamic-func "PQnfields" libpq)
   (list '*)))

(define *PQgetvalue*
  (pointer->procedure
  '*
   (dynamic-func "PQgetvalue" libpq)
   (list '* int int)))

(define (PQgetvalue result row column)
  (pointer->string (*PQgetvalue* result row column)))

(define (psql/open connection-string)
  (*PQconnectdb* (string->pointer connection-string)))

(define (db/result-columns result index)
  (define (-db/result-columns result row-index column-index columns)
    (if (>= column-index columns)
        '()
        (cons (PQgetvalue result row-index column-index)
              (-db/result-columns result row-index (1+ column-index) columns))))
  (-db/result-columns result index 0 (*PQnfields* result)))

(define (db/result-rows result rows)
  (define (-db/result-rows result index rows)
    (if (>= index rows)
        '()
        (cons (db/result-columns result index)
              (-db/result-rows result (1+ index) rows))))
  (-db/result-rows result 0 rows))

(define* (db/query db query
				   #:key
				   (parameters '())
				   (row-handler identity))
  "TODO: Still need to support parameters. Ignore the indexed thing for now."
  (format #t "Querying: ~a~%" query)
  (let* ((result (*PQexec* db (string->pointer query)))
         (rows (*PQntuples* result)))
    (map-in-order row-handler
         (if (> rows 0)
             (let ((result-rows (db/result-rows result rows)))
               (format #t "Results: ~s~%" result-rows)
               result-rows)
             '()))))

(define (db/apply-ddl db)
  (let ((ddl (read-lines db/ddl-file-path)))
    (map
     (lambda (ddl-query)
       
	     (db/query db ddl-query))
     ddl)))

(define (db/close db)
  (format #t "Closing the psql database...~%")
  (*PQfinish* db)
  (format #t "Closed the psql database!~%"))

(define (db/open connection-string)
  "TODO: Migrate to PSQL"
  (format #t "Bringing up the ~s PSQL database...~%" connection-string)
  (let* ((db (psql/open connection-string)))
    (format #t "PSQL database is running: ~a -> ~%" db)
    db))

(define-syntax-rule (db/with (db-binding db-path) body ...)
  "TODO: Migrate to PSQL"
  (let ((db-binding (db/open db-path)))
    (let ((result (begin body ...)))
      (db/close db-binding)
      result)))

(define (db/test)
  (db/with (db db/test-connection-string)
           (db/query db
                     "CREATE TABLE IF NOT EXISTS foo (id INTEGER PRIMARY KEY, bar TEXT);")
           (db/query db
                     "DELETE FROM foo;")
           (db/query db
                     "INSERT INTO foo (bar) VALUES ('something');")
           (db/query db
                     "SELECT * FROM foo WHERE id > 0;")))

;; DOCS: https://doxygen.postgresql.org/structpg__conn.html#pub-attribs
;; char * 	pghost
;; char * 	pghostaddr
;; char * 	pgport
;; char * 	connect_timeout
;; char * 	pgtcp_user_timeout
;; char * 	client_encoding_initial
;; char * 	pgoptions
;; char * 	appname
;; char * 	fbappname
;; char * 	dbName
;; char * 	replication
;; char * 	pguser
;; char * 	pgpass
;; char * 	pgpassfile
;; char * 	channel_binding
;; char * 	keepalives
;; char * 	keepalives_idle
;; char * 	keepalives_interval
;; char * 	keepalives_count
;; char * 	sslmode
;; char * 	sslnegotiation
;; char * 	sslcompression
;; char * 	sslkey
;; char * 	sslcert
;; char * 	sslpassword
;; char * 	sslcertmode
;; char * 	sslrootcert
;; char * 	sslcrl
;; char * 	sslcrldir
;; char * 	sslsni
;; char * 	requirepeer
;; char * 	gssencmode
;; char * 	krbsrvname
;; char * 	gsslib
;; char * 	gssdelegation
;; char * 	ssl_min_protocol_version
;; char * 	ssl_max_protocol_version
;; char * 	target_session_attrs
;; char * 	require_auth
;; char * 	load_balance_hosts
;; bool 	cancelRequest
;; FILE * 	Pfdebug
;; int 	traceFlags
;; PGNoticeHooks 	noticeHooks
;; PGEvent * 	events
;; int 	nEvents
;; int 	eventArraySize
;; ConnStatusType 	status
;; PGAsyncStatusType 	asyncStatus
;; PGTransactionStatusType 	xactStatus
;; char 	last_sqlstate [6]
;; bool 	options_valid
;; bool 	nonblocking
;; PGpipelineStatus 	pipelineStatus
;; bool 	partialResMode
;; bool 	singleRowMode
;; int 	maxChunkSize
;; char 	copy_is_binary
;; int 	copy_already_done
;; PGnotify * 	notifyHead
;; PGnotify * 	notifyTail
;; int 	nconnhost
;; int 	whichhost
;; pg_conn_host * 	connhost
;; char * 	connip
;; PGcmdQueueEntry * 	cmd_queue_head
;; PGcmdQueueEntry * 	cmd_queue_tail
;; PGcmdQueueEntry * 	cmd_queue_recycle
;; pgsocket 	sock
;; SockAddr 	laddr
;; SockAddr 	raddr
;; ProtocolVersion 	pversion
;; int 	sversion
;; bool 	auth_req_received
;; bool 	password_needed
;; bool 	gssapi_used
;; bool 	sigpipe_so
;; bool 	sigpipe_flag
;; bool 	write_failed
;; char * 	write_err_msg
;; bool 	auth_required
;; uint32 	allowed_auth_methods
;; bool 	client_finished_auth
;; char 	current_auth_response
;; PGTargetServerType 	target_server_type
;; PGLoadBalanceType 	load_balance_type
;; bool 	try_next_addr
;; bool 	try_next_host
;; int 	naddr
;; int 	whichaddr
;; AddrInfo * 	addr
;; bool 	send_appname
;; int 	be_pid
;; int 	be_key
;; pgParameterStatus * 	pstatus
;; int 	client_encoding
;; bool 	std_strings
;; PGTernaryBool 	default_transaction_read_only
;; PGTernaryBool 	in_hot_standby
;; PGVerbosity 	verbosity
;; PGContextVisibility 	show_context
;; PGlobjfuncs * 	lobjfuncs
;; pg_prng_state 	prng_state
;; char * 	inBuffer
;; int 	inBufSize
;; int 	inStart
;; int 	inCursor
;; int 	inEnd
;; char * 	outBuffer
;; int 	outBufSize
;; int 	outCount
;; int 	outMsgStart
;; int 	outMsgEnd
;; PGdataValue * 	rowBuf
;; int 	rowBufLen
;; PGresult * 	result
;; bool 	error_result
;; PGresult * 	saved_result
;; const pg_fe_sasl_mech * 	sasl
;; void * 	sasl_state
;; int 	scram_sha_256_iterations
;; uint8 	allowed_enc_methods
;; uint8 	failed_enc_methods
;; uint8 	current_enc_method
;; bool 	ssl_in_use
;; bool 	ssl_handshake_started
;; bool 	ssl_cert_requested
;; bool 	ssl_cert_sent
;; PQExpBufferData 	errorMessage
;; int 	errorReported
;; PQExpBufferData 	workBuffer

;;;;;;;;;;;;;;;;;;;;;;;
;; CONNECTION - PSQL ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; PQconnectdb
;;     PGconn *PQconnectdb(const char *conninfo);
;;
;;     Makes a new connection to the database server.
;;     This function opens a new database connection using the parameters taken from the string conninfo.
;;     The passed string can be empty to use all default parameters, or it can contain one or more parameter settings separated by whitespace, or it can contain a URI. See Section 31.1.1 for details.

;; PQsetdbLogin
;;     PGconn *PQsetdbLogin(const char *pghost,
;;                          const char *pgport,
;;                          const char *pgoptions,
;;                          const char *pgtty,
;;                          const char *dbName,
;;                          const char *login,
;;                          const char *pwd);
;;     Makes a new connection to the database server.

;; PQconninfo Parse
;;     PQconninfoOption *PQconninfoParse(const char *conninfo, char **errmsg);

;;     Returns parsed connection options from the provided connection string.
;;     Parses a connection string and returns the resulting options as an array; or returns NULL if there is a problem with the connection string. This function can be used to extract the PQconnectdb options in the provided connection string. The return value points to an array of PQconninfoOption structures, which ends with an entry having a null keyword pointer
;;     All legal options will be present in the result array, but the PQconninfoOption for any option not present in the connection string will have val set to NULL; default values are not inserted.
;;     If errmsg is not NULL, then *errmsg is set to NULL on success, else to a malloc'd error string explaining the problem. (It is also possible for *errmsg to be set to NULL and the function to return NULL; this indicates an out-of-memory condition.)
;;     After processing the options array, free it by passing it to PQconninfoFree. If this is not done, some memory is leaked for each call to PQconninfoParse. Conversely, if an error occurs and errmsg is not NULL, be sure to free the error string using PQfreemem.

;; PQfinish
;;     void PQfinish(PGconn *conn);

;;     Closes the connection to the server. Also frees memory used by the PGconn object.
;;     Note that even if the server connection attempt fails (as indicated by PQstatus), the application should call PQfinish to free the memory used by the PGconn object. The PGconn pointer must not be used again after PQfinish has been called.

;; PQreset
;;     void PQreset(PGconn *conn);

;;     Resets the communication channel to the server.
;;     This function will close the connection to the server and attempt to reestablish a new connection to the same server, using all the same parameters previously used. This might be useful for error recovery if a working connection is lost.

;; PQresetStart
;; PQresetPoll
;;     int PQresetStart(PGconn *conn);
;;     PostgresPollingStatusType PQresetPoll(PGconn *conn);

;;     Reset the communication channel to the server, in a nonblocking manner.
;;     These functions will close the connection to the server and attempt to reestablish a new connection to the same server, using all the same parameters previously used. This can be useful for error recovery if a working connection is lost. They differ from PQreset (above) in that they act in a nonblocking manner. These functions suffer from the same restrictions as PQconnectStartParams, PQconnectStart and PQconnectPoll.
;;     To initiate a connection reset, call PQresetStart. If it returns 0, the reset has failed. If it returns 1, poll the reset using PQresetPoll in exactly the same way as you would create the connection using PQconnectPoll.

;; PQpingParams
;;     PGPing PQpingParams(const char * const *keywords,
;;                         const char * const *values,
;;                         int expand_dbname);

;;     PQpingParams reports the status of the server. It accepts connection parameters identical to those of PQconnectdbParams, described above. It is not necessary to supply correct user name, password, or database name values to obtain the server status; however, if incorrect values are provided, the server will log a failed connection attempt.
;;     The function returns one of the following values:

;;     PQPING_OK
;;         The server is running and appears to be accepting connections.
;;     PQPING_REJECT
;;         The server is running but is in a state that disallows connections (startup, shutdown, or crash recovery).
;;     PQPING_NO_RESPONSE
;;         The server could not be contacted. This might indicate that the server is not running, or that there is something wrong with the given connection parameters (for example, wrong port number), or that there is a network connectivity problem (for example, a firewall blocking the connection request).
;;     PQPING_NO_ATTEMPT
;;         No attempt was made to contact the server, because the supplied parameters were obviously incorrect or there was some client-side problem (for example, out of memory).

;; PQping
;;     PGPing PQping(const char *conninfo);

;;     PQping reports the status of the server. It accepts connection parameters identical to those of PQconnectdb, described above. It is not necessary to supply correct user name, password, or database name values to obtain the server status; however, if incorrect values are provided, the server will log a failed connection attempt.
;;     The return values are the same as for PQpingParams.

;;;;;;;;;;;;;;;;;;;;;;
;; Execution - PSQL ;;
;;;;;;;;;;;;;;;;;;;;;;

;; PQexec
;;     PGresult *PQexec(PGconn *conn, const char *command);

;;     Submits a command to the server and waits for the result.
;;     Returns a PGresult pointer or possibly a null pointer. A non-null pointer will generally be returned except in out-of-memory conditions or serious errors such as inability to send the command to the server. The PQresultStatus function should be called to check the return value for any errors (including the value of a null pointer, in which case it will return PGRES_FATAL_ERROR). Use PQerrorMessage to get more information about such errors.
;; The command string can include multiple SQL commands (separated by semicolons). Multiple queries sent in a single PQexec call are processed in a single transaction, unless there are explicit BEGIN/COMMIT commands included in the query string to divide it into multiple transactions. Note however that the returned PGresult structure describes only the result of the last command executed from the string. Should one of the commands fail, processing of the string stops with it and the returned PGresult describes the error condition.

;; PQexecParams
;;     PGresult *PQexecParams(PGconn *conn,
;;                            const char *command,
;;                            int nParams,
;;                            const Oid *paramTypes,
;;                            const char * const *paramValues,
;;                            const int *paramLengths,
;;                            const int *paramFormats,
;;                            int resultFormat);
    
;;     Submits a command to the server and waits for the result, with the ability to pass parameters separately from the SQL command text.
;;     PQexecParams is like PQexec, but offers additional functionality: parameter values can be specified separately from the command string proper, and query results can be requested in either text or binary format. PQexecParams is supported only in protocol 3.0 and later connections; it will fail when using protocol 2.0.

;;     The function arguments are:

;;     conn
;;         The connection object to send the command through.
;;     command
;;         The SQL command string to be executed. If parameters are used, they are referred to in the command string as $1, $2, etc.
;;     nParams
;;         The number of parameters supplied; it is the length of the arrays paramTypes[], paramValues[], paramLengths[], and paramFormats[]. (The array pointers can be NULL when nParams is zero.)
;;     paramTypes[]
;;         Specifies, by OID, the data types to be assigned to the parameter symbols. If paramTypes is NULL, or any particular element in the array is zero, the server infers a data type for the parameter symbol in the same way it would do for an untyped literal string.
;;     paramValues[]
;;         Specifies the actual values of the parameters. A null pointer in this array means the corresponding parameter is null; otherwise the pointer points to a zero-terminated text string (for text format) or binary data in the format expected by the server (for binary format).
;;     paramLengths[]
;;         Specifies the actual data lengths of binary-format parameters. It is ignored for null parameters and text-format parameters. The array pointer can be null when there are no binary parameters.
;;     paramFormats[]
;;         Specifies whether parameters are text (put a zero in the array entry for the corresponding parameter) or binary (put a one in the array entry for the corresponding parameter). If the array pointer is null then all parameters are presumed to be text strings.
;;         Values passed in binary format require knowledge of the internal representation expected by the backend. For example, integers must be passed in network byte order. Passing numeric values requires knowledge of the server storage format, as implemented in src/backend/utils/adt/numeric.c::numeric_send() and src/backend/utils/adt/numeric.c::numeric_recv().
;;     resultFormat
;;         Specify zero to obtain results in text format, or one to obtain results in binary format. (There is not currently a provision to obtain different result columns in different formats, although that is possible in the underlying protocol.)

;; The primary advantage of PQexecParams over PQexec is that parameter values can be separated from the command string, thus avoiding the need for tedious and error-prone quoting and escaping.
;; Unlike PQexec, PQexecParams allows at most one SQL command in the given string. (There can be semicolons in it, but not more than one nonempty command.) This is a limitation of the underlying protocol, but has some usefulness as an extra defense against SQL-injection attacks.
;;     Tip: Specifying parameter types via OIDs is tedious, particularly if you prefer not to hard-wire particular OID values into your program. However, you can avoid doing so even in cases where the server by itself cannot determine the type of the parameter, or chooses a different type than you want. In the SQL command text, attach an explicit cast to the parameter symbol to show what data type you will send. For example:
;;     SELECT * FROM mytable WHERE x = $1::bigint;
;;     This forces parameter $1 to be treated as bigint, whereas by default it would be assigned the same type as x. Forcing the parameter type decision, either this way or by specifying a numeric type OID, is strongly recommended when sending parameter values in binary format, because binary format has less redundancy than text format and so there is less chance that the server will detect a type mismatch mistake for you.

;; PQprepare
;;     PGresult *PQprepare(PGconn *conn,
;;                         const char *stmtName,
;;                         const char *query,
;;                         int nParams,
;;                         const Oid *paramTypes);

;;     Submits a request to create a prepared statement with the given parameters, and waits for completion.
;;     PQprepare creates a prepared statement for later execution with PQexecPrepared. This feature allows commands that will be used repeatedly to be parsed and planned just once, rather than each time they are executed. PQprepare is supported only in protocol 3.0 and later connections; it will fail when using protocol 2.0.

;;     The function creates a prepared statement named stmtName from the query string, which must contain a single SQL command. stmtName can be "" to create an unnamed statement, in which case any pre-existing unnamed statement is automatically replaced; otherwise it is an error if the statement name is already defined in the current session. If any parameters are used, they are referred to in the query as $1, $2, etc. nParams is the number of parameters for which types are pre-specified in the array paramTypes[]. (The array pointer can be NULL when nParams is zero.) paramTypes[] specifies, by OID, the data types to be assigned to the parameter symbols. If paramTypes is NULL, or any particular element in the array is zero, the server assigns a data type to the parameter symbol in the same way it would do for an untyped literal string. Also, the query can use parameter symbols with numbers higher than nParams; data types will be inferred for these symbols as well. (See PQdescribePrepared for a means to find out what data types were inferred.)

;;     As with PQexec, the result is normally a PGresult object whose contents indicate server-side success or failure. A null result indicates out-of-memory or inability to send the command at all. Use PQerrorMessage to get more information about such errors.

;; Prepared statements for use with PQexecPrepared can also be created by executing SQL PREPARE statements. Also, although there is no libpq function for deleting a prepared statement, the SQL DEALLOCATE statement can be used for that purpose.

;; PQexecPrepared
;;     PGresult *PQexecPrepared(PGconn *conn,
;;                              const char *stmtName,
;;                              int nParams,
;;                              const char * const *paramValues,
;;                              const int *paramLengths,
;;                              const int *paramFormats,
;;                              int resultFormat);

;;     Sends a request to execute a prepared statement with given parameters, and waits for the result.
;;     PQexecPrepared is like PQexecParams, but the command to be executed is specified by naming a previously-prepared statement, instead of giving a query string. This feature allows commands that will be used repeatedly to be parsed and planned just once, rather than each time they are executed. The statement must have been prepared previously in the current session. PQexecPrepared is supported only in protocol 3.0 and later connections; it will fail when using protocol 2.0.

;;     The parameters are identical to PQexecParams, except that the name of a prepared statement is given instead of a query string, and the paramTypes[] parameter is not present (it is not needed since the prepared statement's parameter types were determined when it was created).

;; PQresultStatus
;;     ExecStatusType PQresultStatus(const PGresult *res);

;;     Returns the result status of the command.
;;     PQresultStatus can return one of the following values:

;;     PGRES_EMPTY_QUERY
;;         The string sent to the server was empty.
;;     PGRES_COMMAND_OK
;;         Successful completion of a command returning no data.
;;     PGRES_TUPLES_OK
;;         Successful completion of a command returning data (such as a SELECT or SHOW).
;;     PGRES_COPY_OUT
;;         Copy Out (from server) data transfer started.
;;     PGRES_COPY_IN
;;         Copy In (to server) data transfer started.
;;     PGRES_BAD_RESPONSE
;;         The server's response was not understood.
;;     PGRES_NONFATAL_ERROR
;;         A nonfatal error (a notice or warning) occurred.
;;     PGRES_FATAL_ERROR
;;         A fatal error occurred.
;;     PGRES_COPY_BOTH
;;         Copy In/Out (to and from server) data transfer started. This feature is currently used only for streaming replication, so this status should not occur in ordinary applications.
;;     PGRES_SINGLE_TUPLE
;;         The PGresult contains a single result tuple from the current command. This status occurs only when single-row mode has been selected for the query (see Section 31.5).
;;     If the result status is PGRES_TUPLES_OK or PGRES_SINGLE_TUPLE, then the functions described below can be used to retrieve the rows returned by the query. Note that a SELECT command that happens to retrieve zero rows still shows PGRES_TUPLES_OK. PGRES_COMMAND_OK is for commands that can never return rows (INSERT or UPDATE without a RETURNING clause, etc.). A response of PGRES_EMPTY_QUERY might indicate a bug in the client software.
;;     A result of status PGRES_NONFATAL_ERROR will never be returned directly by PQexec or other query execution functions; results of this kind are instead passed to the notice processor (see Section 31.12).
;; PQresStatus
;;     char *PQresStatus(ExecStatusType status);

;;     Converts the enumerated type returned by PQresultStatus into a string constant describing the status code. The caller should not free the result.
;; PQresultErrorMessage
;;     char *PQresultErrorMessage(const PGresult *res);

;;     Returns the error message associated with the command, or an empty string if there was no error.
;;     If there was an error, the returned string will include a trailing newline. The caller should not free the result directly. It will be freed when the associated PGresult handle is passed to PQclear.
;;     Immediately following a PQexec or PQgetResult call, PQerrorMessage (on the connection) will return the same string as PQresultErrorMessage (on the result). However, a PGresult will retain its error message until destroyed, whereas the connection's error message will change when subsequent operations are done. Use PQresultErrorMessage when you want to know the status associated with a particular PGresult; use PQerrorMessage when you want to know the status from the latest operation on the connection.
;; PQresultErrorField
;;     char *PQresultErrorField(const PGresult *res, int fieldcode);

;;     Returns an individual field of an error report.
;;     fieldcode is an error field identifier; see the symbols listed below. NULL is returned if the PGresult is not an error or warning result, or does not include the specified field. Field values will normally not include a trailing newline. The caller should not free the result directly. It will be freed when the associated PGresult handle is passed to PQclear.
;;     The following field codes are available:

;;     PG_DIAG_SEVERITY
;;         The severity; the field contents are ERROR, FATAL, or PANIC (in an error message), or WARNING, NOTICE, DEBUG, INFO, or LOG (in a notice message), or a localized translation of one of these. Always present.
;;     PG_DIAG_SQLSTATE
;;         The SQLSTATE code for the error. The SQLSTATE code identifies the type of error that has occurred; it can be used by front-end applications to perform specific operations (such as error handling) in response to a particular database error. For a list of the possible SQLSTATE codes, see Appendix A. This field is not localizable, and is always present.
;;     PG_DIAG_MESSAGE_PRIMARY
;;         The primary human-readable error message (typically one line). Always present.
;;     PG_DIAG_MESSAGE_DETAIL
;;         Detail: an optional secondary error message carrying more detail about the problem. Might run to multiple lines.
;;     PG_DIAG_MESSAGE_HINT
;;         Hint: an optional suggestion what to do about the problem. This is intended to differ from detail in that it offers advice (potentially inappropriate) rather than hard facts. Might run to multiple lines.
;;     PG_DIAG_STATEMENT_POSITION
;;         A string containing a decimal integer indicating an error cursor position as an index into the original statement string. The first character has index 1, and positions are measured in characters not bytes.
;;     PG_DIAG_INTERNAL_POSITION
;;         This is defined the same as the PG_DIAG_STATEMENT_POSITION field, but it is used when the cursor position refers to an internally generated command rather than the one submitted by the client. The PG_DIAG_INTERNAL_QUERY field will always appear when this field appears.
;;     PG_DIAG_INTERNAL_QUERY
;;         The text of a failed internally-generated command. This could be, for example, a SQL query issued by a PL/pgSQL function.
;;     PG_DIAG_CONTEXT
;;         An indication of the context in which the error occurred. Presently this includes a call stack traceback of active procedural language functions and internally-generated queries. The trace is one entry per line, most recent first.
;;     PG_DIAG_SCHEMA_NAME
;;         If the error was associated with a specific database object, the name of the schema containing that object, if any.
;;     PG_DIAG_TABLE_NAME
;;         If the error was associated with a specific table, the name of the table. (Refer to the schema name field for the name of the table's schema.)
;;     PG_DIAG_COLUMN_NAME
;;         If the error was associated with a specific table column, the name of the column. (Refer to the schema and table name fields to identify the table.)
;;     PG_DIAG_DATATYPE_NAME
;;         If the error was associated with a specific data type, the name of the data type. (Refer to the schema name field for the name of the data type's schema.)
;;     PG_DIAG_CONSTRAINT_NAME
;;         If the error was associated with a specific constraint, the name of the constraint. Refer to fields listed above for the associated table or domain. (For this purpose, indexes are treated as constraints, even if they weren't created with constraint syntax.)
;;     PG_DIAG_SOURCE_FILE
;;         The file name of the source-code location where the error was reported.
;;     PG_DIAG_SOURCE_LINE
;;         The line number of the source-code location where the error was reported.
;;     PG_DIAG_SOURCE_FUNCTION
;;         The name of the source-code function reporting the error.
;;         Note: The fields for schema name, table name, column name, data type name, and constraint name are supplied only for a limited number of error types; see Appendix A. Do not assume that the presence of any of these fields guarantees the presence of another field. Core error sources observe the interrelationships noted above, but user-defined functions may use these fields in other ways. In the same vein, do not assume that these fields denote contemporary objects in the current database.
;;     The client is responsible for formatting displayed information to meet its needs; in particular it should break long lines as needed. Newline characters appearing in the error message fields should be treated as paragraph breaks, not line breaks.
;;     Errors generated internally by libpq will have severity and primary message, but typically no other fields. Errors returned by a pre-3.0-protocol server will include severity and primary message, and sometimes a detail message, but no other fields.
;;     Note that error fields are only available from PGresult objects, not PGconn objects; there is no PQerrorField function.
;; PQclear
;;     void PQclear(PGresult *res);

;;     Frees the storage associated with a PGresult. Every command result should be freed via PQclear when it is no longer needed.
;;     You can keep a PGresult object around for as long as you need it; it does not go away when you issue a new command, nor even if you close the connection. To get rid of it, you must call PQclear. Failure to do this will result in memory leaks in your application.

;; PQntuples
;;     int PQntuples(const PGresult *res);

;;     Returns the number of rows (tuples) in the query result. (Note that PGresult objects are limited to no more than INT_MAX rows, so an int result is sufficient.)
;; PQnfields
;;     int PQnfields(const PGresult *res);

;;     Returns the number of columns (fields) in each row of the query result.
;; PQfname
;;     char *PQfname(const PGresult *res,
;;                   int column_number);

;;     Returns the column name associated with the given column number. Column numbers start at 0. The caller should not free the result directly. It will be freed when the associated PGresult handle is passed to PQclear.
;;     NULL is returned if the column number is out of range.
;; PQfnumber
;;     int PQfnumber(const PGresult *res,
;;                   const char *column_name);

;;     Returns the column number associated with the given column name.
;;     -1 is returned if the given name does not match any column.
;;     The given name is treated like an identifier in an SQL command, that is, it is downcased unless double-quoted. For example, given a query result generated from the SQL command:
;;     SELECT 1 AS FOO, 2 AS "BAR";
;;     we would have the results:
;;     PQfname(res, 0)              foo
;;     PQfname(res, 1)              BAR
;;     PQfnumber(res, "FOO")        0
;;     PQfnumber(res, "foo")        0
;;     PQfnumber(res, "BAR")        -1
;;     PQfnumber(res, "\"BAR\"")    1
;; PQftable
;;     Oid PQftable(const PGresult *res,
;;                  int column_number);

;;     Returns the OID of the table from which the given column was fetched. Column numbers start at 0.
;;     InvalidOid is returned if the column number is out of range, or if the specified column is not a simple reference to a table column, or when using pre-3.0 protocol. You can query the system table pg_class to determine exactly which table is referenced.
;;     The type Oid and the constant InvalidOid will be defined when you include the libpq header file. They will both be some integer type.
;; PQftablecol
;;     int PQftablecol(const PGresult *res,
;;                     int column_number);

;;     Returns the column number (within its table) of the column making up the specified query result column. Query-result column numbers start at 0, but table columns have nonzero numbers.
;;     Zero is returned if the column number is out of range, or if the specified column is not a simple reference to a table column, or when using pre-3.0 protocol.
;; PQfformat
;;     int PQfformat(const PGresult *res,
;;                   int column_number);

;;     Returns the format code indicating the format of the given column. Column numbers start at 0.
;;     Format code zero indicates textual data representation, while format code one indicates binary representation. (Other codes are reserved for future definition.)
;; PQftype
;;     Oid PQftype(const PGresult *res,
;;                 int column_number);

;;     Returns the data type associated with the given column number. The integer returned is the internal OID number of the type. Column numbers start at 0.
;;     You can query the system table pg_type to obtain the names and properties of the various data types. The OIDs of the built-in data types are defined in the file include/server/catalog/pg_type.h in the install directory.
;; PQfmod
;;     int PQfmod(const PGresult *res,
;;                int column_number);

;;     Returns the type modifier of the column associated with the given column number. Column numbers start at 0.
;;     The interpretation of modifier values is type-specific; they typically indicate precision or size limits. The value -1 is used to indicate "no information available". Most data types do not use modifiers, in which case the value is always -1.
;; PQfsize
;;     int PQfsize(const PGresult *res,
;;                 int column_number);

;;     Returns the size in bytes of the column associated with the given column number. Column numbers start at 0.
;;     PQfsize returns the space allocated for this column in a database row, in other words the size of the server's internal representation of the data type. (Accordingly, it is not really very useful to clients.) A negative value indicates the data type is variable-length.
;; PQbinaryTuples
;;     int PQbinaryTuples(const PGresult *res);

;;     Returns 1 if the PGresult contains binary data and 0 if it contains text data.
;;     This function is deprecated (except for its use in connection with COPY), because it is possible for a single PGresult to contain text data in some columns and binary data in others. PQfformat is preferred. PQbinaryTuples returns 1 only if all columns of the result are binary (format 1).
;; PQgetvalue
;;     char *PQgetvalue(const PGresult *res,
;;                      int row_number,
;;                      int column_number);

;;     Returns a single field value of one row of a PGresult. Row and column numbers start at 0. The caller should not free the result directly. It will be freed when the associated PGresult handle is passed to PQclear.
;;     For data in text format, the value returned by PQgetvalue is a null-terminated character string representation of the field value. For data in binary format, the value is in the binary representation determined by the data type's typsend and typreceive functions. (The value is actually followed by a zero byte in this case too, but that is not ordinarily useful, since the value is likely to contain embedded nulls.)
;;     An empty string is returned if the field value is null. See PQgetisnull to distinguish null values from empty-string values.
;;     The pointer returned by PQgetvalue points to storage that is part of the PGresult structure. One should not modify the data it points to, and one must explicitly copy the data into other storage if it is to be used past the lifetime of the PGresult structure itself.
;; PQgetisnull
;;     int PQgetisnull(const PGresult *res,

;;     Tests a field for a null value. Row and column numbers start at 0.
;;                     int row_number,
;;                     int column_number);
;;     This function returns 1 if the field is null and 0 if it contains a non-null value. (Note that PQgetvalue will return an empty string, not a null pointer, for a null field.)
;; PQgetlength
;;     int PQgetlength(const PGresult *res,
;;                     int row_number,
;;                     int column_number);

;;     Returns the actual length of a field value in bytes. Row and column numbers start at 0.
;;     This is the actual data length for the particular data value, that is, the size of the object pointed to by PQgetvalue. For text data format this is the same as strlen(). For binary format this is essential information. Note that one should not rely on PQfsize to obtain the actual data length.
;; PQnparams
;;     int PQnparams(const PGresult *res);

;;     Returns the number of parameters of a prepared statement.
;;     This function is only useful when inspecting the result of PQdescribePrepared. For other types of queries it will return zero.
;; PQparamtype
;;     Oid PQparamtype(const PGresult *res, int param_number);

;;     Returns the data type of the indicated statement parameter. Parameter numbers start at 0.
;;     This function is only useful when inspecting the result of PQdescribePrepared. For other types of queries it will return zero.

(export dbi-open
	dbi-close
	dbi-query
	dbi-get_row
	dbi-get_status)

(begin
  ;; TODO: Make this a little more ergonomic
  (define psql-bv (make-bytevector (sizeof (list '* '*))))

  (define-foreign-type (read-PGconn write-PGconn)
    (pghost '*)
    (pghostaddr '*))

  (write-PGconn psql-bv
                0
                (string->pointer "test1")
                (string->pointer "test2"))
  (let ((conn (call-with-values (lambda () (read-PGconn psql-bv 0))
    list)))
    ;; TODO: Conver the list to an alist
    (format #t "hostname: ~a~%" (pointer->string (car conn)))))
