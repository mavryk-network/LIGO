#include <stdlib.h>
#include "api.h"
typedef struct entrypoint_tuple {
  int parameter; // TODO: implement
  __wasi_iovec_t* storage;
} entrypoint_tuple;

typedef struct tezos_operations {
  uint32_t length;

} tezos_operations;

typedef struct entrypoint_result {
  tezos_operations* operations;
  __wasi_ciovec_t* storage;
} entrypoint_result;

void _start() {
  // storage file information
  __wasi_filestat_t *fstat;
  __wasi_errno_t err_code1 = __wasi_path_filestat_get(3, 0, "storage.byte", fstat);
  if (err_code1 != __WASI_ERRNO_SUCCESS) {
    __wasi_proc_exit(2);
    return;
  } 
  
  // allocate memory for storage
  __wasi_iovec_t* iovs = malloc(fstat->size);
  if (iovs == NULL) {
    __wasi_proc_exit(3);
    return;
  }

  // read storage into allocated memory
  __wasi_size_t* nread;
  __wasi_errno_t err_code2 = __wasi_fd_read(0, iovs, 1, nread);
  if (err_code2 != __WASI_ERRNO_SUCCESS) {
    __wasi_proc_exit(4);
    return;
  }
  
  // call smart contract entrypoint
  entrypoint_tuple et = {
    .parameter = 1,
    .storage   = iovs
  };
  entrypoint_result *er;
  int err_code3 = entrypoint(et, er);
  if (err_code3 != 0) {
    __wasi_proc_exit(5);
    return;
  } 
  
  // open storage file for writing
  __wasi_fd_t *fd;
  int err_code4 = __wasi_path_open(3, 0, "storage.byte", 0, __WASI_RIGHTS_FD_WRITE & __WASI_RIGHTS_PATH_OPEN, 0, 0, fd);
  if (err_code4 != 0) {
    __wasi_proc_exit(6);
    return;
  }

  printf("okay: %i\n", er->storage);
    
  // write returned storage from contract to storage file
  __wasi_size_t *retptr1;
  int err_code5 = __wasi_fd_write(*fd, er->storage, 1, retptr1);
  if (err_code5 !=0) {
    __wasi_proc_exit(7);
    return;
  }

  // close the file descriptor to the file
  int err_code6 = __wasi_fd_close(*fd);
  if (err_code6 != 0) {
    __wasi_proc_exit(8);
    return;
  } 
  
  __wasi_proc_exit(EXIT_SUCCESS);
}