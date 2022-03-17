#include <stdlib.h>
#include "../../../vendors/gmp/gmp-6.2.1/gmp.h"
#include "api.h"

extern void __load();
extern uint32_t __save(uint32_t src_addr);

typedef struct entrypoint_tuple {
  int parameter; // TODO: implement
  __wasi_iovec_t* storage;
} entrypoint_tuple;

typedef struct tezos_operations {
  uint32_t length;

} tezos_operations;

typedef struct entrypoint_result {
  tezos_operations* operations;

// /**
//  * A region of memory for scatter/gather writes.
//  */
// typedef struct __wasi_ciovec_t {
//     /**
//      * The address of the buffer to be written.
//      */
//     const uint8_t * buf;

//     /**
//      * The length of the buffer to be written.
//      */
//     __wasi_size_t buf_len;

// } __wasi_ciovec_t;

  __wasi_ciovec_t* storage;
} entrypoint_result;

void _start() {
  // storage file information
  __wasi_fd_t *fd1;
  int err_code423 = __wasi_path_open(3, 0, "storage.byte", 0, __WASI_RIGHTS_FD_READ | __WASI_RIGHTS_FD_FILESTAT_GET | __WASI_RIGHTS_PATH_OPEN, 0, 0, fd1);
  if (err_code423 != __WASI_ERRNO_SUCCESS) {
    __wasi_proc_exit(63);
    return;
  }

  __wasi_filestat_t *fstat;
  __wasi_errno_t err_code1 = __wasi_fd_filestat_get(*fd1, fstat);
  if (err_code1 != __WASI_ERRNO_SUCCESS) {
    __wasi_proc_exit(9);
    return;
  } 

  //   int err_code623 = __wasi_fd_close(fd1);
  // if (err_code623 != __WASI_ERRNO_SUCCESS) {
  //   __wasi_proc_exit(92);
  //   return;
  // } 
  
  // allocate memory for storage
  printf("Allocate: %i\n", fstat->size);

  int* alloc = malloc(fstat->size);
  if (alloc == NULL) {
    __wasi_proc_exit(3);
    return;
  }
  __wasi_iovec_t iovs = { 
    .buf = alloc,
    .buf_len = fstat->size
  };
  
  // __wasi_fd_t *fd2;
  int err_code42 = __wasi_path_open(3, 0, "storage.byte", 0, __WASI_RIGHTS_FD_READ | __WASI_RIGHTS_FD_FILESTAT_GET | __WASI_RIGHTS_PATH_OPEN, 0, 0, fd1);
  if (err_code42 != __WASI_ERRNO_SUCCESS) {
    __wasi_proc_exit(6);
    return;
  }


  // read storage into allocated memory
  __wasi_size_t nread;
  __wasi_errno_t err_code2 = __wasi_fd_read(*fd1, &iovs, 1, &nread);

  if (err_code2 != __WASI_ERRNO_SUCCESS) {
    __wasi_proc_exit(4);
    
    return;
  }

  printf("Bytes read: %lu\n", nread);

  

  // call generated `__load` to fix pointers in storage
  __load();


gmp_printf("The storage contents before: %Zd\n", alloc);



  // call smart contract entrypoint
  entrypoint_tuple et = {
    .parameter = 1,
    .storage   = &iovs
  };
  // int loc = ;
  entrypoint_result *er = malloc(3);
  int err_code3 = entrypoint(&et, &er);
  if (err_code3 != __WASI_ERRNO_SUCCESS) {
    __wasi_proc_exit(5);
    return;
  } 

  
  // open storage file for writing
  
  int err_code4 = __wasi_path_open(3, 0, "storage.byte", 0, __WASI_RIGHTS_FD_WRITE | __WASI_RIGHTS_PATH_OPEN, 0, 0, fd1);
  if (err_code4 != __WASI_ERRNO_SUCCESS) {
    __wasi_proc_exit(6);
    return;
  }

  int a = er->storage;
  gmp_printf("The storage contents after 1: %Zd\n", (int*)a);

  int x = __save(a);
  printf("location1: %i\n", x);
  
  // int *x = er->storage;
  
  gmp_printf("The storage contents after 2: %Zd\n", x);
    gmp_printf("x2\n");
  /*
    TODO: implement proper handling of storage. Challenges:
    - pointers
    - data structures
  */
  __wasi_ciovec_t foo = {
    .buf = x,
    .buf_len = 20 // how to get this number the right way?!
  };
gmp_printf("x3\n");
  // write returned storage from contract to storage file
  __wasi_size_t *retptr1;
  int err_code5 = __wasi_fd_write(*fd1, &foo, 1, retptr1);
  if (err_code5 != __WASI_ERRNO_SUCCESS) {
    __wasi_proc_exit(7);
    return;
  }

  // close the file descriptor to the file
  int err_code6 = __wasi_fd_close(fd1);
  if (err_code6 != __WASI_ERRNO_SUCCESS) {
    __wasi_proc_exit(8);
    return;
  } 
  gmp_printf("x5\n");
  __wasi_proc_exit(EXIT_SUCCESS);
}