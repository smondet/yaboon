
/* To get nanosleep: */
#define  _POSIX_C_SOURCE 199309L


/*
 * Sockets and posix calls
 */
#include <time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <errno.h>

/* 
 * Classic includes:
 */
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

/*
 * Caml wrapping includes:
 *
 */
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/custom.h>

#include <caml/unixsupport.h>

CAMLprim value
unsafix_socket(value domain, value type, value proto)
{
    CAMLparam3(domain, type, proto);
    int ret = -42;
    ret = socket(Int_val(domain), Int_val(type), Int_val(proto));
    CAMLreturn(Val_int(ret));

}

CAMLprim value
unsafix_setsockopt_int(value filedes, value level, value optname, value optval)
{

    CAMLparam4(filedes, level, optname, optval);
    int val = Int_val(optval);
    int len = sizeof(int);
    int result;
    result = setsockopt(Int_val(filedes), Int_val(level),
            Int_val(optname), (const char *) &val, len);

    CAMLreturn(Val_int(result));
}

CAMLprim value
unsafix_getsockopt_int(value filedes, value level, value optname)
{

    CAMLparam3(filedes, level, optname);
    CAMLlocal1(ret);
    int val = 0;
    socklen_t len = sizeof(int);
    int result;
    result = getsockopt(Int_val(filedes), Int_val(level),
            Int_val(optname), (void *) &val, &len);
    if (result == 0) {
        ret = Val_int(val);
    } else {
        ret = Val_int(result);
    }

    CAMLreturn(ret);
}

/*
CAMLprim value
unsafix_setsockopt_str(value filedes, value level, value optname, value optval)
{

    CAMLparam4(filedes, level, optname, optval);
    int val = Int_val(optval);
    int len = sizeof(int);
    int result;
    result = setsockopt(Int_val(filedes), Int_val(level),
            Int_val(optname), (const char *) &val, len);

    CAMLreturn(Val_int(result));
}
*/
#define GETSOCKOPT_STR_BUFFER_SIZE 200

CAMLprim value
unsafix_getsockopt_str(value filedes, value level, value optname, value optlength)
{

    CAMLparam4(filedes, level, optname, optlength);
    CAMLlocal2(ret,val);
    socklen_t len = Int_val(optlength);
    char buf[GETSOCKOPT_STR_BUFFER_SIZE];
    int result;
    unsigned int i;

    if (len >= GETSOCKOPT_STR_BUFFER_SIZE) {
        ret = hash_variant("optlength_overflow");
        CAMLreturn(ret);
    }

    result = getsockopt(Int_val(filedes), Int_val(level),
            Int_val(optname), (void *) buf, &len);
    ret = caml_alloc(2, 0);
    if (result == 0) {
        val = caml_alloc_string(Int_val(optlength));
        for (i = 0; i < len; i++) {
            Byte(val, i) = buf[i];
        }

        Store_field(ret, 0, hash_variant("ok"));
        Store_field(ret, 1, val);
    } else {
        Store_field(ret, 0, hash_variant("error"));
        Store_field(ret, 1, result);
    }

    CAMLreturn(ret);
}



CAMLprim value
unsafix_nanosleep(value secs, value nanos) 
{
    CAMLparam2(secs, nanos);
    CAMLlocal2(returned_status, returned_time);
    struct timespec input_spec, output_rem;
    int ret = 0;
    input_spec.tv_sec = Int_val(secs);
    input_spec.tv_nsec = Int_val(nanos);
    ret = nanosleep(&input_spec, &output_rem);
    /* printf("ret: %d, errno: %d EFAULT:%d\n", ret, errno, EFAULT); */
    if (ret == 0) {
        returned_status = hash_variant("ok");
    } else {
        switch (errno) {
            case EFAULT:
                returned_status = hash_variant("error_EFAULT");
                break;
            case EINVAL:
                returned_status = hash_variant("error_EINVAL");
                break;
            case EINTR:
                returned_status = caml_alloc(2, 0);
                returned_time = caml_alloc(2, 0);
                Store_field(returned_time, 0, Val_int(output_rem.tv_sec));
                Store_field(returned_time, 1, Val_int(output_rem.tv_nsec));
                Store_field(returned_status, 0, hash_variant("interrupted"));
                Store_field(returned_status, 1, returned_time);
                break;
            default:
                caml_failwith("can't manage this errno value");
        }
    }
    /* printf("returned_status: %d\n", returned_status); */
    CAMLreturn(returned_status);
}

CAMLprim value
unsafix_current_errno(value unit) 
{
    CAMLparam1(unit);
    CAMLreturn(unix_error_of_code(errno));
}

