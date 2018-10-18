/*-
 * Copyright (c) 2018 Alexandre Joannou
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <math.h>

// general helper functions
inline unsigned long long div_up(unsigned long long a, unsigned long long b)
{
  return (a/b)+((a%b)?1:0);
}

int hexToInt(char c)
{
  if(c >= '0' && c <= '9') return c - '0';
  if(c >= 'a' && c <= 'f') return c - 'a' + 10;
  if(c >= 'A' && c <= 'F') return c - 'A' + 10;
  return 0;
}

void print_mem(const unsigned char * const mem, unsigned long long from, unsigned long long to, unsigned long long word_sz)
{
  int i, j;
  int shft = (int) log2(word_sz);
  unsigned long long from_align = (from << shft) >> shft;
  unsigned long long to_align = (to << shft) >> shft;
  for (i = from_align; i < from_align + to_align; i+= word_sz)
  {
    printf("0x%04x: 0x", i);
    for (j = word_sz-1; j >= 0; j--)
    {
      printf("%02x",mem[i+j]);
    }
    printf("\n");
  }
}

// HEX file loader functions
#define MAX_WIDTH 256
char * parse_hex_line(char * line)
{
  char cmt[3] = "//";
  // check for comments
  if (strncmp(line, cmt, 2) == 0) return NULL;
  // strip end comments
  char * tok = strtok(line, cmt);
  // trim leading whitespaces
  while(isspace(*tok)) tok++;
  // trim trailing whitespaces
  char * last = tok + strlen(tok) - 1;
  while(last > tok && isspace(*last)) last--;
  *(last + 1) = '\0';
  return tok;
}

unsigned long long write_hex_to_buff(char * hexline, unsigned char * buff)
{
  int i;
  unsigned long long pos = 0;
  for (i = strlen(hexline) - 1; i >=0; i -= 2)
    buff[pos++] = hexToInt(hexline[i]) + (hexToInt(hexline[i-1])<<4);
  return pos;
}

void load_hex (const char * filename, unsigned char * buff, unsigned long long buff_sz)
{
  // open file
  FILE * fp = fopen(filename, "r");
  if (fp == NULL) {
    fprintf(stderr, "Error opening %s: %s\n", filename, strerror(errno));
    exit(-1);
  }
  int done = 0;
  // fetch first line to determine the line width
  char line[MAX_WIDTH];
  char * hexline;
  if(fgets((char*)&line, MAX_WIDTH, fp) == NULL) done = 1;
  hexline = parse_hex_line(line);
  unsigned long long width = (hexline) ? div_up(strlen(hexline),2) : 0;
  // get rest of the file
  unsigned long long space_left = buff_sz;
  while (!done && space_left - width > 0)
  {
    // fold in previous line
    if (hexline)
    {
      unsigned long long bytes_written = write_hex_to_buff(hexline, buff);
      space_left -= bytes_written;
      buff += bytes_written;
    }
    // get next line and check for end
    if (fgets((char*)&line, MAX_WIDTH, fp) == NULL) done = 1;
    hexline = parse_hex_line(line);
    width = (hexline) ? div_up(strlen(hexline),2) : 0;
  }
  fclose(fp);
}


// functions to be used in Bluespec

typedef struct {
  unsigned char * const data;
  unsigned long long size;
} mem_t;

unsigned long long mem_create(unsigned int * memSize)
{
  mem_t * m = (mem_t *) malloc (sizeof(mem_t));
  m->size = *((unsigned long long*) memSize);
  *(unsigned char **)&m->data = (unsigned char *) malloc (m->size * sizeof(unsigned char));
  printf("---- allocated memory size of 0x%0llx\n", m->size);
  return (unsigned long long) m;
}

void mem_init(unsigned long long mem_ptr, char * hexfile, unsigned long long offset)
{
  mem_t * m = (mem_t *) mem_ptr;
  load_hex(hexfile, &(m->data[offset]), m->size - offset);
  //printf("loaded hex file...\n");
  //print_mem(m->data, 0, 2048, 4);
}

void mem_zero(unsigned long long mem_ptr)
{
  mem_t * m = (mem_t *) mem_ptr;
  explicit_bzero((void *) m->data, m->size);
}

void mem_read(unsigned int * ret_data, unsigned long long mem_ptr, unsigned int * addr_ptr, unsigned int * size_ptr)
{
  mem_t * m = (mem_t *) mem_ptr;
  unsigned long long a = (unsigned long long) *addr_ptr;
  unsigned long long s = (unsigned long long) *size_ptr;
  unsigned long long i;
  for (i = 0; i < s; i++)
  {
    ((unsigned char*)(ret_data))[i] = m->data[(a+i)%(m->size)];
    if (a+i >= m->size) printf("---- @ 0x%0llx >= memory size 0x%0llx, reading from @ 0x%0llx instead\n", a+i, m->size, (a+i)%(m->size));
  }
  //printf("---- reading @ 0x%04x, %d bytes, ret_data = 0x%08x\n", a, s, *ret_data);
  //print_mem(m->data, a - 8, a + 8, 4);
}

void mem_write(unsigned long long mem_ptr, unsigned int * addr_ptr, unsigned int * size_ptr, unsigned int * be_ptr, unsigned int * data_ptr)
{
  mem_t * m = (mem_t *) mem_ptr;
  unsigned long long a  = (unsigned long long) *addr_ptr;
  unsigned long long s  = (unsigned long long) *size_ptr;
  unsigned long long be = (unsigned long long) *be_ptr;
  unsigned char * d     = (unsigned char *) data_ptr;

  unsigned long long i;
  for (i = 0; i < s; i++)
  {
    if (be & 0x1) m->data[(a+i)%(m->size)] = d[i];
    be >>= 1;
    if (a+i >= m->size) printf("---- @ 0x%0llx >= memory size 0x%0llx, writing to @ 0x%0llx instead\n", a+i, m->size, (a+i)%(m->size));
  }

  //printf("---- writing @ 0x%04x, %d bytes, be = %08x, data = 0x%08x\n", a, s, be, *d);
  //print_mem(m->data, a - 8, a + 8, 4);
}

void mem_clean(unsigned long long mem_ptr)
{
    mem_t * m = (mem_t*) mem_ptr;
    free(m);
}
