#define CAML_INTERNALS
#ifndef CAML_RUNTIME_5
#define CAML_NAME_SPACE
#endif

#include <caml/mlvalues.h>
#include <caml/domain_state.h>
#include <caml/signals.h>
#if CAML_RUNTIME_5
#include <caml/minor_gc.h>
#include <caml/camlatomic.h>
#endif

CAMLprim value request_minor_gc(value v) {
  Caml_state->requested_minor_gc = 1;
  Caml_state->requested_major_slice = 1;
  /*
    This is massively unsafe in multicore but the polling
    tests are only run in a single domain, so we're probably
    good.
  */
#if CAML_RUNTIME_5
  Caml_state->young_limit = (uintnat)Caml_state->young_end;
#else
  caml_something_to_do = 1;
  Caml_state->young_limit = Caml_state->young_alloc_end;
#endif

  return Val_unit;
}

CAMLprim value minor_gcs(value v) {
#if CAML_RUNTIME_5
  return Val_long(atomic_load(&caml_minor_collections_count));
#else
  return Val_long(Caml_state->stat_minor_collections);
#endif
}
