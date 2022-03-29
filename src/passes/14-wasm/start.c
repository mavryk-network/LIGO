#include <stdlib.h>
#include <stdio.h>
#include "../../../vendors/gmp/gmp-6.2.1/gmp.h"
#include "api.h"

extern void __load(uint8_t * addr);
extern void __save(__wasi_ciovec_t* src_addr, __wasi_ciovec_t* result);


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

extern int entrypoint(entrypoint_tuple* et, entrypoint_result** er);


void report_error (int err_code) {
  if (err_code == __WASI_ERRNO_ACCES) {
    printf("__WASI_ERRNO_ACCES\n");
  }
  else if (err_code == __WASI_ERRNO_NOTCAPABLE) {
    printf("__WASI_ERRNO_ACCES\n");
  }    
  else if (err_code == __WASI_ERRNO_ROFS) {
    printf("__WASI_ERRNO_ROFS\n");
  }
  else if (err_code == __WASI_ERRNO_RANGE) {
    printf("__WASI_ERRNO_RANGE\n");
  }
  else if (err_code == __WASI_ERRNO_PIPE) {
    printf("__WASI_ERRNO_PIPE\n");
  }
  else if (err_code == __WASI_ERRNO_OVERFLOW) {
    printf("__WASI_ERRNO_OVERFLOW\n");
  }
  else if (err_code == __WASI_ERRNO_NOTTY) {
    printf("__WASI_ERRNO_NOTTY\n");
  }
  else if (err_code == __WASI_ERRNO_NOSPC) {
    printf("__WASI_ERRNO_NOSPC\n");
  }
  else if (err_code == __WASI_ERRNO_NOMEM) {
    printf("__WASI_ERRNO_NOMEM\n");
  }
  else if (err_code == __WASI_ERRNO_NOLCK) {
    printf("__WASI_ERRNO_NOLCK\n");
  }
  else if (err_code == __WASI_ERRNO_NOENT) {
    printf("__WASI_ERRNO_NOENT\n");
  }
  else if (err_code == __WASI_ERRNO_NOBUFS) {
    printf("__WASI_ERRNO_NOBUFS\n");
  }
  else if (err_code == __WASI_ERRNO_MULTIHOP) {
    printf("__WASI_ERRNO_MULTIHOP\n");
  }
  else if (err_code == __WASI_ERRNO_MSGSIZE) {
    printf("__WASI_ERRNO_MSGSIZE\n");
  }
  else if (err_code == __WASI_ERRNO_MFILE) {
    printf("__WASI_ERRNO_MFILE\n");
  }
  else if (err_code == __WASI_ERRNO_IO) {
    printf("__WASI_ERRNO_IO\n");
  }
  else if (err_code == __WASI_ERRNO_INVAL) {
    printf("__WASI_ERRNO_INVAL\n");
  }
  else if (err_code == __WASI_ERRNO_INPROGRESS) {
    printf("__WASI_ERRNO_INPROGRESS\n");
  }
  else if (err_code == __WASI_ERRNO_ILSEQ) {
    printf("__WASI_ERRNO_ILSEQ\n");
  }
  else if (err_code == __WASI_ERRNO_FAULT) {
    printf("__WASI_ERRNO_FAULT\n");
  }
  else if (err_code == __WASI_ERRNO_DEADLK) {
    printf("__WASI_ERRNO_DEADLK\n");
  }
  else if (err_code == __WASI_ERRNO_BUSY) {
    printf("__WASI_ERRNO_BUSY\n");
  }
  else if (err_code == __WASI_ERRNO_BADMSG) {
    printf("__WASI_ERRNO_BADMSG\n");
  }
  else if (err_code == __WASI_ERRNO_BADF) {
    printf("__WASI_ERRNO_BADF\n");
  }
  else if (err_code == __WASI_ERRNO_AGAIN) {
    printf("__WASI_ERRNO_AGAIN\n");
  }
  else if (err_code == __WASI_ERRNO_AFNOSUPPORT) {
    printf("__WASI_ERRNO_AFNOSUPPORT\n");
  }
  else if (err_code == __WASI_ERRNO_ADDRNOTAVAIL) {
    printf("__WASI_ERRNO_ADDRNOTAVAIL\n");
  }
  else if (err_code == __WASI_ERRNO_ACCES) {
    printf("__WASI_ERRNO_ACCES\n");
  }
  else if (err_code == __WASI_ERRNO_2BIG) {
    printf("__WASI_ERRNO_2BIG\n");
  }
  else {
    printf("something else: good luck.\n");
  }
}

void _start() {
  // storage file information
  printf("a0\n");

  __wasi_fd_t *fd1;
  __wasi_errno_t err_code = __wasi_path_open(3, 0, "storage.byte", __WASI_OFLAGS_CREAT, __WASI_RIGHTS_FD_FILESTAT_GET | __WASI_RIGHTS_PATH_OPEN, 0, 0, fd1);
  __wasi_fd_t origin_fd1 = *fd1;
  if (err_code != __WASI_ERRNO_SUCCESS) {
    report_error(err_code);
    __wasi_proc_exit(1);
    return;
  }

  __wasi_filestat_t *fstat;
  err_code = __wasi_fd_filestat_get(*fd1, fstat);
  if (err_code != __WASI_ERRNO_SUCCESS) {
    report_error(err_code);
    __wasi_proc_exit(2);
    return;
  } 

  printf("a1\n");
  
  // allocate the file size into memory
  uint8_t * alloc = malloc(fstat->size);
  if (alloc == NULL) {
    report_error(err_code);
    __wasi_proc_exit(3);
    return;
  }

  printf("a1\n");

  // read the file into memory
  __wasi_iovec_t iovs = { 
    .buf = alloc,
    .buf_len = fstat->size
  };
  __wasi_iovec_t *iovs2 = &iovs;
  

  printf("a1\n");

  err_code = __wasi_fd_close(origin_fd1);
  if (err_code != __WASI_ERRNO_SUCCESS) {
    report_error(err_code);
    __wasi_proc_exit(8);
    return;
  } 

  __wasi_fd_t *fd2;
  err_code = __wasi_path_open(3, 0, "storage.byte", 0, __WASI_RIGHTS_FD_READ, 0, 0, fd2);
  __wasi_fd_t origin_fd2 = *fd2;
  if (err_code != __WASI_ERRNO_SUCCESS) {
    report_error(err_code);
    __wasi_proc_exit(4);
    return;
  }


printf("a1\n");

  // read storage into allocated memory
  __wasi_size_t nread;
  err_code = __wasi_fd_read(*fd2, &iovs, 1, &nread);
  if (err_code != __WASI_ERRNO_SUCCESS) {
    report_error(err_code);
    __wasi_proc_exit(4);
    return;
  }

  uint8_t * given_storage = iovs2->buf;

  int *z1 = (int*)((char*)given_storage);
  int *z2 = (int*)((char*)given_storage + 4);
  int *z3 = (int*)((char*)given_storage + 8);
  int *z4 = (int*)((char*)given_storage + 12);
  int *z5 = (int*)((char*)given_storage + 16);
  printf("z1: %i\n", *z1);
  printf("z2: %i\n", *z2);
  printf("z3: %i\n", *z3);
  printf("z4: %i\n", *z4);
  printf("z5: %i\n", *z5);
  printf("gs: %i\n", given_storage);

  // call generated `__load` function which corrects the pointers
  __load(given_storage);
  
printf("a3x\n");

  int *a1 = (int*)((char*)given_storage);
  int *a2 = (int*)((char*)given_storage + 4);
  int *a3 = (int*)((char*)given_storage + 8);
  int *a4 = (int*)((char*)given_storage + 12);
  int *a5 = (int*)((char*)given_storage + 16);
  int *a6 = (int*)((char*)given_storage + 32);
  // printf("a1: %i\n", a1);
  printf("a1: %i\n", *a1);
  printf("a2: %i\n", *a2);
  printf("a3: %i\n", *a3);
  printf("a4: %i\n", *a4);
  int ax = (int*)((char*)given_storage);

  // 02 00 00 00 01 00 00 00 0C 00 00 00 FB 07 00 00
  // 08 00 00 00 18 00 00 00 02 00 00 00 01 00 00 00 14 00 00 00 FB 07 00 00 02 00 00 00 01 00 00 00
  // gmp_printf("The storage contents after 1: %Zd\n", a3 + given_storage );
  // gmp_printf("The storage contents after 1: %Zd\n", a1 );
  // gmp_printf("The storage contents after 1: %Zd\n", a5 );

  // printf("a3\n");

  __wasi_iovec_t * buf = (__wasi_iovec_t *)iovs2->buf;

  // call smart contract entrypoint
  entrypoint_tuple et = {
    .parameter = 1,
    .storage   = buf
  };
  entrypoint_result *er = malloc(3);
  err_code = entrypoint(&et, &er);
  if (err_code != __WASI_ERRNO_SUCCESS) {
    report_error(err_code);
    __wasi_proc_exit(5);
    return;
  } 

printf("a4\n");
  __wasi_ciovec_t* storage = er->storage;

  // for debugging purposes, should be removed once there is a better way to print storage data
  

printf("a5\n");
  __wasi_ciovec_t result;

  /**
   * Call the generated `__save` function.
   * 
   * This function compresses the data and changes the pointers so the file is
   * easy to load.
   */
  __save(storage, &result);



  printf("The size: %i \n", result.buf_len);

  err_code = __wasi_fd_close(origin_fd2);
  if (err_code != __WASI_ERRNO_SUCCESS) {
    report_error(err_code);
    __wasi_proc_exit(8);
    return;
  } 

  // enable once load works...
  __wasi_fd_t *fd3;
  err_code = __wasi_path_open(3, 0, "storage.byte", 0, __WASI_RIGHTS_FD_WRITE, 0, 0, fd3);
  __wasi_fd_t origin_fd3 = *fd3;
  if (err_code != __WASI_ERRNO_SUCCESS) {
    report_error(err_code);
    __wasi_proc_exit(6);
    return;
  }

  // write to storage
  __wasi_size_t *retptr1;
  err_code = __wasi_fd_write(*fd3, &result, 1, retptr1);
  if (err_code != __WASI_ERRNO_SUCCESS) {
    report_error(err_code);
    __wasi_proc_exit(7);
    return;
  }

  // close the file handler
  err_code = __wasi_fd_close(origin_fd3);
  if (err_code != __WASI_ERRNO_SUCCESS) {
    report_error(err_code);
    __wasi_proc_exit(8);
    return;
  } 

  // should we free the memory as well here?
  free(er);
  free(alloc);
  // TODO: free everything that was allocated in the generated code

  // pfew everything went well it seems
  __wasi_proc_exit(EXIT_SUCCESS);
}