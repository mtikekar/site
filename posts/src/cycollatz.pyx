import sys

cdef int collatzLen(int a0):
    cdef unsigned long a = a0
    cdef int length = 0
    while a != 1:
        a = (a if a%2 == 0 else 3*a+1) / 2
        length += 1
    return length

def maxLen(max_a0):
    cdef int max_length = 0, longest = 0, a0, length
    for a0 in xrange(1, max_a0 + 1):
        length = collatzLen(a0)
        if length > max_length:
            max_length = length
            longest = a0
    return max_length, longest

if __name__ == '__main__':
    max_a0 = int(sys.argv[1])
    print maxLen(max_a0)
