#include <stdio.h>

int main(int argc, char **argv) {
   int max_a0 = atoi(argv[1]); 
   int longest = 0, max_len = 0;
   int a0, len;
   unsigned long a;
   
   for (a0 = 1; a0 <= max_a0; a0++) {
      a = a0;
      len = 0;
      
      while (a != 1) {
         len++;
         a = ((a%2==0)? a : 3*a+1)/2;
      }
      
      if (len > max_len) {
         max_len = len;
         longest = a0;
      }
   }
   printf("(%d, %d)\n", max_len, longest);
   return 0;
}
