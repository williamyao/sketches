#include <stdio.h>
#include <stdlib.h>

void array_swap(int* array, int index1, int index2)
{
  int temp = array[index1];
  array[index1] = array[index2];
  array[index2] = temp;
}

void quicksort(int* to_sort, int start, int end)
{
  if(start < end){
    int pivot = to_sort[(start + end) / 2];
    int less_bin = start;
    int more_bin = end;

    to_sort[(start + end) / 2] = -1;

    while(less_bin != more_bin){
      if(to_sort[less_bin] == -1){
	array_swap(to_sort, less_bin, more_bin);
      } else if(to_sort[less_bin] < pivot){
	less_bin++;
      } else{
	array_swap(to_sort, less_bin, more_bin);
	more_bin--;
      }
    }

    to_sort[less_bin] = pivot;

    quicksort(to_sort, start, less_bin - 1);
    quicksort(to_sort, less_bin + 1, end);
  }
}

void print_array(int* array, int length)
{
  int i;
  printf("[");
  for(i = 0; i < length; i++){
    printf("%d, ", array[i]);
  }
  printf("]");
}


int* generate_random_array(int length)
{
  int* array = malloc(sizeof(int) * length);
  int i;

  for(i = 0; i < length; i++){
    array[i] = (rand() % 100) + 1;
  }

  return array;
}

int main()
{
  int* array = generate_random_array(32);

  printf("Before sorting: ");
  print_array(array, 32);

  printf("\n");

  quicksort(array, 0, 31);

  printf("After sorting: ");
  print_array(array, 32);

  return 0;
}
