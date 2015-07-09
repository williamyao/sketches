#include <stdint.h>
#include <stdio.h>

typedef struct node_slop
{
  uint32_t key;
  struct node_slop* left;
  struct node_slop* right;
  uint32_t data;
} node_slop;

typedef struct node_noslop
{
  struct node_noslop* left;
  struct node_noslop* right;
  uint32_t key;
  uint32_t data;
} node_noslop;

int main()
{
  printf("sizeof(node_slop): %lu\n", sizeof(node_slop));
  printf("sizeof(node_noslop): %lu\n", sizeof(node_noslop));
}
