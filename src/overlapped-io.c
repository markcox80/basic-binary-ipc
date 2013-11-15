#include <rpc.h>
#include <mswsock.h>
#include <stdio.h>

#define STOLEN_WSAID_GETACCEPTEXSOCKADDRS {0xb5367df2,0xcbac,0x11cf,{0x95,0xca,0x00,0x80,0x5f,0x48,0xa1,0x92}}
#define STOLEN_WSAID_ACCEPTEX {0xb5367df1,0xcbac,0x11cf,{0x95,0xca,0x00,0x80,0x5f,0x48,0xa1,0x92}}
#define STOLEN_WSAID_CONNECTEX {0x25a207b9,0xddf3,0x4660,{0x8e,0xe9,0x76,0xe5,0x8c,0x74,0x06,0x3e}}

#ifndef WSAID_GETACCEPTEXSOCKADDRS
#define WSAID_GETACCEPTEXSOCKADDRS STOLEN_WSAID_GETACCEPTEXSOCKADDRS
#endif

#ifndef WSAID_ACCEPTEX
#define WSAID_ACCEPTEX STOLEN_WSAID_ACCEPTEX
#endif

#ifndef WSAID_CONNECTEX
#define WSAID_CONNECTEX STOLEN_WSAID_CONNECTEX
#endif

void
output_guid(const char *name, GUID guid)
{
  size_t i;
  unsigned char *p = (unsigned char *)(&guid);

  printf(";; %s\n", name);  
  printf("#(");
  for (i = 0; i < sizeof(GUID); i++) {
    printf("%u",p[i]);
    if (i < (sizeof(GUID) - 1))
      printf(" ");
    
  }
  printf(")\n"); 
}

int
main(int argc, char **argv)
{
  {
    GUID guid = WSAID_GETACCEPTEXSOCKADDRS;
    output_guid("WSAID_GETACCEPTEXSOCKADDRS", guid);
  }
  
  {
    GUID guid = WSAID_ACCEPTEX;
    output_guid("WSAID_ACCEPTEX", guid);
  }

  {
    GUID guid = WSAID_CONNECTEX;
    output_guid("WSAID_CONNECTEX", guid);
  }


  return 0;
}
