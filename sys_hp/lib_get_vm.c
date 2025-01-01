#include <stdio.h>
/*
	Fortran callable memory allocator

	Called as :
		ier = lib_get_vm (size,pointer)

	where : size is an integer size of memory to allocate
		pointer is an integer to return the pointer into

*/
lib_get_vm(size,pointer)
int *size,*pointer;
{
    char *area, *malloc();

    area = malloc(*size);
    *pointer = (int)area;
    if(area == NULL) return 0;
    return 1;
}

/*
	Fortran callable memory deallocator

	Called as :
		ier = lib_free_vm (size,pointer)

	where : size is an integer size of memory to deallocate (not used)
		pointer is an integer that contains the pointer

*/
lib_free_vm(size,pointer)
int *size,*pointer;
{
    char *area;

    area = (char *)*pointer;
    free(area);
    return 1;
}
