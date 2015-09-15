/*
 A merge sort implementation that only copies the
 array to be sorted once.

 It's possible to do it in place, but unless you're
 doing this in parallel, there's probably not much of a
 point.
*/

#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef bool (*comparator)(const void* a, const void* b);

void* copy_array(void* array, uint64_t elements, uint64_t size)
{
    void* new_array = malloc(elements * size);

    if(new_array) {
        memcpy(new_array, array, elements * size);
        return new_array;
    } else {
        return NULL;
    }
}

void* pointer_add_bytes(void* pointer, uint64_t bytes) {
    uint8_t* byte_pointer = (uint8_t*) pointer;

    return (void*) (byte_pointer + bytes);
}

void _merge(void* reference_array,
            void* sort_into_array,
            uint64_t start1,
            uint64_t end1,
            uint64_t start2,
            uint64_t end2,
            uint64_t size,
            comparator compare)
{
    uint64_t index_sort_into = start1; /* this is internal, we can assume */
                                       /* that the to-merges are contiguous */

    uint64_t index1 = start1;
    uint64_t index2 = start2;

    void* current1 = pointer_add_bytes(reference_array, size * index1);
    void* current2 = pointer_add_bytes(reference_array, size * index2);
    void* to_sort_into = pointer_add_bytes(sort_into_array, size * index_sort_into);
    
    while(index1 <= end1 && index2 <= end2) {
        if(compare(current1, current2)) {
            memcpy(to_sort_into, current1, size);
            index1++;
            current1 = pointer_add_bytes(current1, size);
        } else {
            memcpy(to_sort_into, current2, size);
            index2++;
            current2 = pointer_add_bytes(current2, size);
        }
        to_sort_into = pointer_add_bytes(to_sort_into, size);
    }

    if(index1 > end1) {
        for(; index2 <= end2; index2++) {
            memcpy(to_sort_into, current2, size);
            
            to_sort_into = pointer_add_bytes(to_sort_into, size);
            current2 = pointer_add_bytes(current2, size);
        }
    } else {
        for(; index1 <= end1; index1++) {
            memcpy(to_sort_into, current1, size);

            to_sort_into = pointer_add_bytes(to_sort_into, size);
            current1 = pointer_add_bytes(current1, size);
        }
    }
}

void _merge_sort(void* reference_array,
                 void* sort_into_array,
                 uint64_t start,
                 uint64_t end,
                 uint64_t size,
                 comparator compare)
{
    if(end > start) {
        uint64_t middle = (start / 2) + (end / 2); /* (start + end) / 2 could overflow */

        _merge_sort(sort_into_array, reference_array, start, middle, size, compare);
        _merge_sort(sort_into_array, reference_array, middle + 1, end, size, compare);
        _merge(reference_array, sort_into_array, start, middle, middle + 1, end, size, compare);
    }
}

/*
  array - pointer to the start of the array to sort
  elements - number of elements in the array
  size - size of each element, in bytes
  compare - function to call to compare elements.
     whatever compares the best ends up at the front of the array
*/
void merge_sort(void* array,
                uint64_t elements,
                uint64_t size,
                comparator compare)
{
    void* reference_array = copy_array(array, elements, size);

    if(reference_array) {
        _merge_sort(reference_array, array, 0, elements - 1, size, compare);

        free(reference_array);
    }
}

bool less_than(const void* a, const void* b)
{
    return *((uint64_t*) a) < *((uint64_t*) b);
}

void print_uint64_t_array(uint64_t array[], uint64_t elements)
{
    uint64_t index;

    for(index = 0; index < elements; index++) {
        printf("%llu ", array[index]);
    }

    printf("\n");
}

int main()
{
    uint64_t my_array[] = {5, 4, 40, 4308, 10, 0, 41};
    uint64_t* copied_array = (uint64_t*) copy_array(my_array, 7, sizeof(uint64_t));

    print_uint64_t_array(copied_array, 7);

    merge_sort(my_array, 7, sizeof(uint64_t), less_than);

    print_uint64_t_array(my_array, 7);
    
    return 0;
}
