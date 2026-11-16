#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2026, Benson Muite
All rights reserved.

Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the
following conditions are met:

* Redistributions of source code must retain the above
copyright notice, this list of conditions and the following
disclaimer.

* Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following
disclaimer in the documentation and/or other materials
provided with the distribution.

* Neither the name of the University of British Columbia nor
the names of its contributors may be used to endorse or
promote products derived from this software without specific
prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

;; minimal bindings for the strophe xmpp library

(define strophe:debuglevel 0)
(define (strophe:log level . x)
   (if (>= strophe:debuglevel level) (apply log-system (append (list "strophe: ") x))))

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <strophe.h>

(define strophe:xmpp_initialize (c-lambda ((pointer void)) void "_xmpp_initialize"))
(define strophe:xmpp_shutdown (c-lambda ((pointer void)) void "_xmpp_shutdown"))
(define strophe:xmpp_version_check (c-lambda (int int) int "_xmpp_version_check"))
(define strophe:xmpp_ctx_new (c-lambda ((pointer void) (pointer void)) (pointer void) "_xmpp_ctx_new"))
(define strophe:xmpp_ctx_free (c-lambda ((pointer void)) void "_xmpp_ctx_free"))
(define strophe:xmpp_ctx_set_verbosity (c-lambda ((pointer void) int) void "_xmpp_ctx_set_verbosity"))
(define strophe:xmpp_free (c-lambda ((pointer void) (pointer void)) void "_xmpp_free"))
(define strophe:xmpp_get_default_logger (c-lambda (int) (pointer void) "_xmpp_get_default_logger"))
(define strophe:xmpp_sockopt_cb_keepalive (c-lambda ((pointer void) (pointer void)) int "_xmpp_sockopt_cb_keepalive"))
(define strophe:xmpp_send_error (c-lambda ((pointer void) int char-string) void "_xmpp_send_error"))
(define strophe:xmpp_conn_new (c-lambda ((pointer void)) (pointer void) "_xmpp_conn_new"))
(define strophe:xmpp_conn_clone (c-lambda ((pointer void)) (pointer void) "_xmpp_conn_clone"))
(define strophe::xmpp_conn_release (c-lambda ((pointer void)) int "_xmpp_conn_release"))
(define strophe::xmpp_conn_get_flags (c-lambda ((pointer void)) long "_xmpp_conn_get_flags"))
(define strophe::xmpp_conn_set_flags (c-lambda ((pointer void) long) int "_xmpp_conn_set_flags"))
(define strophe::xmpp_conn_get_jid (c-lambda ((pointer void)) char-string "_xmpp_conn_get_jid"))
(define strophe::xmpp_conn_get_bound_jid (c-lambda ((pointer void)) char-string "_xmpp_con_get_bound_jid"))
(define strophe::xmpp_conn_set_jid (c-lambda ((pointer void) char-string) void "_xmpp_conn_set_jid"))
(define strophe::xmpp_conn_set_cafile (c-lambda ((pointer void) char-string) void "_xmpp_conn_set_cafile"))
(define strophe::xmpp_conn_set_capath (c-lambda ((pointer void) char-string) void "_xmpp_conn_set_capath"))
(define strophe::xmpp_conn_set_certfail_handler (c-lambda ((pointer void) int) void "_xmpp_conn_set_certfail_handler"))
(define strophe::xmpp_conn_get_peer_cert (c-lambda ((pointer void)) (pointer void) "_xmpp_conn_get_peer_cert"))
(define strophe::xmpp_conn_set_password_callback (c-lambda ((pointer void) int (pointer void))
                                                           void "_xmpp_conn_set_password_callback"))
(define strophe::xmpp_conn_set_password_retries (c-lambda ((pointer void) int) void "_xmpp_conn_set_password_retries"))
(define strophe::xmpp_conn_get_keyfile (c-lambda ((pointer void)) char-string "_xmpp_conn_get_keyfile"))
(define strophe::xmpp_conn_set_client_cert (c-lambda ((pointer void) char-string char-string) 
                                                     void "_xmpp_conn_set_client_cert"))
(define strophe::xmpp_conn_cert_xmppaddr_num (c-lambda ((pointer void)) int "_xmpp_conn_cert_xmppaddr_num"))
(define strophe::xmpp_conn_cert_xmppaddr (c-lambda ((pointer void)) int) char-string "_xmpp_conn_cert_xmppaddr"))
(define strophe::xmpp_conn_get_pass (c-lambda ((pointer void)) char-string "_xmpp_conn_get_pass"))
(define strophe::xmpp_conn_set_pass (c-lambda ((pointer void) char-string) void "_xmpp_conn_set_pass"))
(define strophe::xmpp_conn_get_context (c-lambda ((pointer void)) (pointer void) "_xmpp_conn_get_context"))
(define strophe::xmpp_conn_is_secured (c-lambda ((pointer void)) int "_xmpp_conn_is_secured"))
(define strophe::xmpp_conn_set_sockopt_callback (c-lambda ((pointer void) int)
                                                          void "_xmpp_conn_set_sockopt_callback"))
(define strophe::xmpp_conn_is_connecting (c-lambda ((pointer void)) int "_xmpp_conn_is_connecting"))
(define strophe::xmpp_conn_is_connected (c-lambda ((pointer void)) int "_xmpp_conn_is_connected"))
(define strophe::xmpp_conn_is_disconnected (c-lambda ((pointer void)) int "_xmpp_conn_is_disconnected"))
(define strophe::xmpp_conn_send_queue_len (c-lambda ((pointer void)) int "_xmpp_conn_send_queue_len"))

(define strophe::xmpp_conn_send_queue_drop_element (c-lambda ((pointer void) int) xmpp_conn_t *conn,
                                                             char-string "_xmpp_conn_send_queue_drop_element"))

(define strophe::xmpp_conn_set_sm_state (c-lambda ((pointer void) (pointer void)) int "_xmpp_conn_set_sm_state"))
(define strophe::xmpp_conn_get_sm_state (c-lambda ((pointer void)) (pointer void) "_xmpp_conn_get_sm_state"))

(define strophe::xmpp_conn_set_sm_callback (c-lambda ((pointer void) int (pointer void))
                                                     void "_xmpp_conn_set_sm_callback"))
(define strophe::xmpp_conn_restore_sm_state (c-lambda ((pointer void) char-string int)
                                                      int "_xmpp_conn_restore_sm_state"))

(define strophe::xmpp_free_sm_state (c-lambda ((pointer void)) void "_xmpp_free_sm_state"))

(define strophe::xmpp_connect_client (c-lambda ((pointer void) char-string int int (pointer void))
                                               int "_xmpp_connect_client"))

(define strophe::xmpp_connect_component (c-lambda ((pointer void) char-string int (pointer void) (pointer void))
                                                  int "_xmpp_connect_component"))

(define strophe::xmpp_connect_raw (c-lambda ((pointer void) char-string int (pointer void) (pointer void))
                                            int "_xmpp_connect_raw"))
(define strophe::xmpp_conn_open_stream_default (c-lambda ((pointer void)) int "_xmpp_conn_open_stream_default"))
;;(define strophe::int xmpp_conn_open_stream(xmpp_conn_t *conn,
;;                          char **attributes,
;;                          size_t attributes_len);
(define strophe::xmpp_conn_tls_start (c-lambda ((pointer void)) int "_xmpp_conn_tls_start"))

(define strophe::xmpp_disconnect (c-lambda ((pointer void)) void "_xmpp_disconnect"))

(define strophe::xmpp_send (c-lambda ((pointer void) (pointer void)) void "_xmpp_send"))

(define strophe::xmpp_send_raw_string (c-lambda ((pointer void) char-string) void "_xmpp_send_raw_string"))
(define strophe::xmpp_send_raw (c-lambda ((pointer void) char-string int) void "_xmpp_send_raw"))

(define strophe::xmpp_timed_handler_add (c-lambda ((pointer void) int int (pointer void))
                                                  void "_xmpp_timed_handler_add"))
(define strophe::xmpp_timed_handler_delete (c-lambda ((pointer void) int) "_xmpp_timed_handler_delete"))

(define strophe::xmpp_global_timed_handler_add (c-lambda ((pointer void)) int int (pointer void))
                                                         void "_xmpp_global_timed_handler_add"))
(define strophe::xmpp_global_timed_handler_delete (c-lambda ((pointer void) int)
                                                            void "_xmpp_global_timed_handler_delete"))

(define strophe::xmpp_handler_add (c-lambda ((pointer void) int char-string char-string char-string (pointer void))
                                            void "_xmpp_handler_add"))
(define strophe::xmpp_handler_delete (c-lambda ((pointer void) int) void "_xmpp_handler_delete"))

(define strophe::xmpp_id_handler_add (c-lambda ((pointer void) int char-string (pointer void))
                                               void "_xmpp_id_handler_add"))
(define strophe::xmpp_id_handler_delete (c-lambda ((pointer void) int char-string)
                                                  void "_xmpp_id_handler_delete"))

(define strophe::xmpp_stanza_new (c-lambda ((pointer void)) (pointer void) "_xmpp_stanza_new"))
(define strophe::xmpp_stanza_new_from_string (c-lambda ((pointer void) char-string) (pointer void) "_xmpp_stanza_new_from_string"))

(define strophe::xmpp_stanza_clone (c-lambda ((pointer void)) (pointer void) "_xmpp_stanza_clone"))

(define strophe::xmpp_stanza_copy (c-lambda ((pointer void)) (pointer void) "_xmpp_stanza_copy"))

(define strophe::xmpp_stanza_release (c-lambda ((pointer void)) int "_xmpp_stanza_release"))

(define strophe::xmpp_stanza_get_context (c-lambda ((pointer void)) (pointer void) "_xmpp_stanza_get_context"))

(define strophe::xmpp_stanza_is_text (c-lambda ((pointer void)) int "_xmpp_stanza_is_text"))
(define strophe::xmpp_stanza_is_tag (c-lambda ((pointer void)) int "_xmpp_stanza_is_tag"))

;;(define strophe::int xmpp_stanza_to_text(xmpp_stanza_t *stanza, char **buf, size_t *buflen);

(define strophe::xmpp_stanza_get_children (c-lambda ((pointer void)) (pointer void) "_xmpp_stanza_get_children"))
(define strophe::xmpp_stanza_get_child_by_name (c-lambda ((pointer void) char-string)
                                                         (pointer void) "_xmpp_stanza_get_child_by_name"))
(define strophe::xmpp_stanza_get_child_by_ns (c-lambda ((pointer void) char-string)
                                                       (pointer void) "_xmpp_stanza_get_child_by_ns"))
(define strophe::xmpp_stanza_get_child_by_name_and_ns (c-lambda ((pointer void) char-string char-string)
                                                                (pointer void) "_xmpp_stanza_get_child_by_name_and_ns"))
;;(define strophe::xmpp_stanza_t *xmpp_stanza_get_child_by_path(xmpp_stanza_t *stanza, ...);
(define strophe::xmpp_stanza_get_next (c-lambda ((pointer void)) (pointer void) "_xmpp_stanza_get_next"))
(define strophe::xmpp_stanza_add_child (c-lambda ((pointer void) (pointer void)) int "_xmpp_stanza_add_child"))
(define strophe::xmpp_stanza_add_child_ex (c-lambda ((pointer void) (pointer void) int) 
                                                    int "_xmpp_stanza_add_child_ex"))

(define strophe::xmpp_stanza_get_attribute (c-lambda ((pointer void) char-string) char-string "_xmpp_stanza_get_attribute"))
(define strophe::xmpp_stanza_get_attribute_count (c-lambda ((pointer void)) int "_xmpp_stanza_get_attribute_count"))
;;(define strophe::int xmpp_stanza_get_attributes(xmpp_stanza_t *stanza,
;;                               const char **attr,
;;                               int attrlen);
(define strophe::xmpp_stanza_get_text (c-lambda ((pointer void)) char-string "_xmpp_stanza_get_text"))
(define strophe::xmpp_stanza_get_text_ptr (c-lambda ((pointer void)) char-string "_xmpp_stanza_get_text_ptr"))
(define strophe::xmpp_stanza_get_name (c-lambda ((pointer void)) char-string "_xmpp_stanza_get_name"))
(define strophe::xmpp_stanza_set_attribute (c-lambda ((pointer void) char-string char-string)
                                                     int "_xmpp_stanza_set_attribute"))
(define strophe::xmpp_stanza_set_name (c-lambda ((pointer void) char-string) int "_xmpp_stanza_set_name"))
(define strophe::xmpp_stanza_set_text (c-lambda ((pointer void) char-string) int "_xmpp_stanza_set_text"))
(define strophe::xmpp_stanza_set_text_with_size (c-lambda ((pointer void) char-string int)
                                                          int "_xmpp_stanza_set_text_with_size"))
(define strophe::xmpp_stanza_del_attribute (c-lambda ((pointer void) char-string) int "_xmpp_stanza_del_attribute"))

(define strophe::xmpp_stanza_get_ns (c-lambda ((pointer void)) char-string "_xmpp_stanza_get_ns"))
(define strophe::xmpp_stanza_get_type (c-lambda ((pointer void)) char-string "_xmpp_stanza_get_type"))
(define strophe::xmpp_stanza_get_id (c-lambda ((pointer void)) char-string "_xmpp_stanza_get_id"))
(define strophe::xmpp_stanza_get_to (c-lambda ((pointer void)) char-string "_xmpp_stanza_get_to"))
(define strophe::xmpp_stanza_get_from (c-lambda ((pointer void)) char-string "_xmpp_stanza_get_from"))
(define strophe::xmpp_stanza_set_ns (c-lambda ((pointer void) char-string) int "_xmpp_stanza_set_ns"))
(define strophe::xmpp_stanza_set_id (c-lambda ((pointer void) char-string) int "_xmpp_stanza_set_id"))
(define strophe::xmpp_stanza_set_type (c-lambda ((pointer void) char-string) int "_xmpp_stanza_set_type"))
(define strophe::xmpp_stanza_set_to (c-lambda ((pointer void) char-string) int "_xmpp_stanza_set_to"))
(define strophe::xmpp_stanza_set_from (c-lambda ((pointer void) char-string) int "_xmpp_stanza_set_from"))

(define strophe::xmpp_stanza_reply (c-lambda ((pointer void)) (pointer void) "_xmpp_stanza_reply"))
(define strophe::xmpp_stanza_reply_error (c-lambda ((pointer void) char-string char-string char-string)
                                                   (pointer void) "_xmpp_stanza_reply_error"))

(define strophe::xmpp_message_new (c-lambda ((pointer void) char-string char-string char-string)
                                            (pointer void) "_xmpp_message_new"))
(define strophe::xmpp_message_get_body (c-lambda ((pointer void)) char-string "_xmpp_message_get_body"))
(define strophe::xmpp_message_set_body (c-lambda ((pointer void) char-string) int "_xmpp_message_set_body"))

(define strophe::xmpp_iq_new (c-lambda ((pointer void) char-string char-string) (pointer void) "_xmpp_iq_new"))
(define strophe::xmpp_presence_new (c-lambda ((pointer void)) (pointer void) "_xmpp_presence_new"))
(define strophe::xmpp_error_new (c-lambda ((pointer void) int char-string) (pointer void) "_xmpp_error_new"))

(define strophe::xmpp_jid_new (c-lambda ((pointer-void) char-string char-string char-string)
                                        char-string "_xmpp_jid_new"))
(define strophe::xmpp_jid_bare (c-lambda ((pointer-void) char-string) char-string "_xmpp_jid_bare"))
(define strophe::xmpp_jid_node (c-lambda ((pointer-void) char-string) char-string "_xmpp_jid_node"))
(define strophe::xmpp_jid_domain (c-lambda ((pointer-void) char-string) char-string "_xmpp_jid_domain"))
(define strophe::xmpp_jid_resource (c-lambda ((pointer-void) char-string) char-string "_xmpp_jid_resource"))


(define strophe::xmpp_run_once (c-lambda ((pointer void) int) void "_xmpp_run_once"))
(define strophe::xmpp_run (c-lambda ((pointer void)) void "_xmpp_run"))
(define strophe::xmpp_stop (c-lambda ((pointer void)) void "_xmpp_stop"))
(define strophe::xmpp_ctx_set_timeout (c-lambda ((pointer void) int) void "_xmpp_ctx_set_timeout"))


(define strophe::xmpp_tlscert_get_ctx (c-lambda ((pointer void)) (pointer void) "_xmpp_tlscert_get_ctx"))
(define strophe::xmpp_tlscert_get_conn (c-lambda ((pointer void)) (pointer void) "_xmpp_tlscert_get_conn"))
(define strophe::xmpp_tlscert_get_userdata (c-lambda ((pointer void)) void "_xmpp_tlscert_get_userdata"))
(define strophe::xmpp_tlscert_get_pem (c-lambda ((pointer void)) char-string "_xmpp_tlscert_get_pem"))
(define strophe::xmpp_tlscert_get_dnsname (c-lambda ((pointer void) int) char-string "_xmpp_tlscert_get_dnsname"))
(define strophe::xmpp_tlscert_get_string (c-lambda ((pointer void) int)
                                                   char-string "_xmpp_tlscert_get_string"))
(define strophe::xmpp_tlscert_get_description (c-lambda int) char-string "_xmpp_tlscert_get_description"))
(define strophe::xmpp_tlscert_free (c-lambda ((pointer void)) void "_xmpp_tlscert_free"))

(define strophe::xmpp_uuid_gen (c-lambda ((pointer void)) char-string "_xmpp_uuid_gen"))
(define strophe::xmpp_sha1 (c-lambda ((pointer void) char-string int) char-string "_xmpp_sha1"))
(define strophe::xmpp_sha1_digest (c-lambda ((pointer void) int (pointer void))
                                            void "_xmpp_sha1_digest"))

(define strophe::xmpp_sha1_new (c-lambda ((pointer void)) (pointer void) "_xmpp_sha1_new"))
(define strophe::xmpp_sha1_free (c-lambda ((pointer void)) void "_xmpp_sha1_free"))
(define strophe::xmpp_sha1_update (c-lambda ((pointer void) (pointer void) int) void "_xmpp_sha1_update"))
(define strophe::xmpp_sha1_final (c-lambda ((pointer void)) void "_xmpp_sha1_final"))
(define strophe::xmpp_sha1_to_string (c-lambda ((pointer void) char-string int) char-string "_xmpp_sha1_to_string"))
(define strophe::xmpp_sha1_to_string_alloc (c-lambda ((pointer void)) char-string "_xmpp_sha1_to_string_alloc"))
(define strophe::xmpp_sha1_to_digest (c-lambda ((pointer void) (pointer void)) void "_xmpp_sha1_to_digest"))


(define strophe::xmpp_base64_encode (c-lambda ((pointer void) (pointer void) int) char-string "_xmpp_base64_encode"))
(define strophe::xmpp_base64_decode_str (c-lambda ((pointer void) (pointer void) int) char-string "_xmpp_base64_decode_str"))
;;(define strophe::void xmpp_base64_decode_bin(xmpp_ctx_t *ctx,
;;                            const char *base64,
;;                            size_t len,
;;                            unsigned char **out,
;;                            size_t *outlen);
(define strophe::xmpp_rand_new (c-lambda ((pointer void)) (pointer void) "_xmpp_rand_new"))

(define strophe::xmpp_rand_free (c-lambda ((pointer void) (pointer void)) void "_xmpp_rand_free"))

(define strophe::xmpp_rand (c-lambda ((pointer void)) int "_xmpp_rand"))

(define strophe::xmpp_rand_bytes (c-lambda ((pointer void) (pointer void) int) void "_xmpp_rand_bytes"))

(define strophe::xmpp_rand_nonce (c-lambda ((pointer void) char-string int) void "_xmpp_rand_nonce"))

;; eof
