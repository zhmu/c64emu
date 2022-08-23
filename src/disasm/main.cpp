#include <stdio.h>
#include <stdlib.h>

#define INSTR_LEN 4

#define PARAM_NONE          0
#define PARAM_BRANCH        1
#define PARAM_INDIRECTX     2
#define PARAM_INDIRECTY     3
#define PARAM_ZPAGE         4
#define PARAM_ZPAGEX        5
#define PARAM_ZPAGEY        6
#define PARAM_IMM           7
#define PARAM_ACC           8
#define PARAM_ABS           9
#define PARAM_ABSX          10
#define PARAM_ABSY          11
#define PARAM_ABSJUMP       12
#define PARAM_INDIRECTJUMP  13
#define PARAM_ABSOLUTE      14
#define PARAM_ERROR         0xff

#define TEMPSTR_LEN         128

struct INSTRUCTION {
    char name[INSTR_LEN];
    unsigned char params;
};

char*    code;
int      base_offs;
int      code_size;
int      bytes_to_disasm;
int      start_offs;

struct INSTRUCTION instr[0x100] = { { "BRK", PARAM_NONE },          /* 00 */
                                    { "ORA", PARAM_INDIRECTX },     /* 01 */
                                    { "---", PARAM_ERROR },         /* 02 */
                                    { "---", PARAM_ERROR },         /* 03 */
                                    { "---", PARAM_ERROR },         /* 04 */
                                    { "ORA", PARAM_ZPAGE },         /* 05 */
                                    { "ASL", PARAM_ZPAGE },         /* 06 */
                                    { "---", PARAM_ERROR },         /* 07 */
                                    { "PHP", PARAM_NONE  },         /* 08 */
                                    { "ORA", PARAM_IMM   },         /* 09 */
                                    { "ASL", PARAM_ACC   },         /* 0A */
                                    { "---", PARAM_ERROR },         /* 0B */
                                    { "---", PARAM_ERROR },         /* 0C */
                                    { "ORA", PARAM_ABS   },         /* 0D */
                                    { "ASL", PARAM_ABS   },         /* 0E */
                                    { "---", PARAM_ERROR },         /* 0F */
                                    { "BPL", PARAM_BRANCH },        /* 10 */
                                    { "ORA", PARAM_INDIRECTY },     /* 11 */
                                    { "---", PARAM_ERROR },         /* 12 */
                                    { "---", PARAM_ERROR },         /* 13 */
                                    { "---", PARAM_ERROR },         /* 14 */
                                    { "ORA", PARAM_ZPAGEX },        /* 15 */
                                    { "ASL", PARAM_ZPAGEX },        /* 16 */
                                    { "---", PARAM_ERROR },         /* 17 */
                                    { "CLC", PARAM_NONE },          /* 18 */
                                    { "ORA", PARAM_ABSY  },         /* 19 */
                                    { "---", PARAM_ERROR },         /* 1A */
                                    { "---", PARAM_ERROR },         /* 1B */
                                    { "---", PARAM_ERROR },         /* 1C */
                                    { "ORA", PARAM_ABSX  },         /* 1D */
                                    { "ASL", PARAM_ABSX  },         /* 1E */
                                    { "---", PARAM_ERROR },         /* 1F */
                                    { "JSR", PARAM_ABSJUMP },       /* 20 */
                                    { "AND", PARAM_INDIRECTX },     /* 21 */
                                    { "---", PARAM_ERROR },         /* 22 */
                                    { "---", PARAM_ERROR },         /* 23 */
                                    { "BIT", PARAM_ZPAGE },         /* 24 */
                                    { "AND", PARAM_ZPAGE },         /* 25 */
                                    { "ROL", PARAM_ZPAGE },         /* 26 */
                                    { "---", PARAM_ERROR },         /* 27 */
                                    { "PLP", PARAM_ERROR },         /* 28 */
                                    { "AND", PARAM_IMM   },         /* 29 */
                                    { "ROL", PARAM_ACC   },         /* 2A */
                                    { "---", PARAM_ERROR },         /* 2B */
                                    { "BIT", PARAM_ABS   },         /* 2C */
                                    { "AND", PARAM_ABS   },         /* 2D */
                                    { "ROL", PARAM_ABS   },         /* 2E */
                                    { "---", PARAM_ERROR },         /* 2F */
                                    { "BMI", PARAM_BRANCH },        /* 30 */
                                    { "AND", PARAM_INDIRECTY },     /* 31 */
                                    { "---", PARAM_ERROR },         /* 32 */
                                    { "---", PARAM_ERROR },         /* 33 */
                                    { "---", PARAM_ERROR },         /* 34 */
                                    { "AND", PARAM_ZPAGEX },        /* 35 */
                                    { "ROL", PARAM_ZPAGEX },        /* 36 */
                                    { "---", PARAM_ERROR },         /* 37 */
                                    { "SEC", PARAM_NONE },          /* 38 */
                                    { "AND", PARAM_ABSX },          /* 39 */
                                    { "---", PARAM_ERROR },         /* 3A */
                                    { "---", PARAM_ERROR },         /* 3B */
                                    { "---", PARAM_ERROR },         /* 3C */
                                    { "AND", PARAM_ABSX },          /* 3D */
                                    { "ROL", PARAM_ABSX },          /* 3E */
                                    { "---", PARAM_ERROR },         /* 3F */
                                    { "RTI", PARAM_NONE },          /* 40 */
                                    { "EOR", PARAM_INDIRECTX },     /* 41 */
                                    { "---", PARAM_ERROR },         /* 42 */
                                    { "---", PARAM_ERROR },         /* 43 */
                                    { "---", PARAM_ERROR },         /* 44 */
                                    { "EOR", PARAM_ZPAGE },         /* 45 */
                                    { "LSR", PARAM_ZPAGE },         /* 46 */
                                    { "---", PARAM_ERROR },         /* 47 */
                                    { "PHA", PARAM_NONE },          /* 48 */
                                    { "EOR", PARAM_IMM   },         /* 49 */
                                    { "LSR", PARAM_ACC   },         /* 4A */
                                    { "---", PARAM_ERROR },         /* 4B */
                                    { "JMP", PARAM_ABSJUMP },       /* 4C */
                                    { "EOR", PARAM_ABS },           /* 4D */
                                    { "LSR", PARAM_ABS },           /* 4E */
                                    { "---", PARAM_ERROR },         /* 4F */
                                    { "BVC", PARAM_BRANCH },        /* 50 */
                                    { "EOR", PARAM_INDIRECTY },     /* 51 */
                                    { "---", PARAM_ERROR },         /* 52 */
                                    { "---", PARAM_ERROR },         /* 53 */
                                    { "---", PARAM_ERROR },         /* 54 */
                                    { "EOR", PARAM_ZPAGEX },        /* 55 */
                                    { "LSR", PARAM_ZPAGEX },        /* 56 */
                                    { "---", PARAM_ERROR },         /* 57 */
                                    { "CLI", PARAM_NONE },          /* 58 */
                                    { "EOR", PARAM_ABSY },          /* 59 */
                                    { "---", PARAM_ERROR },         /* 5A */
                                    { "---", PARAM_ERROR },         /* 5B */
                                    { "---", PARAM_ERROR },         /* 5C */
                                    { "EOR", PARAM_ABSX },          /* 5D */
                                    { "LSR", PARAM_ABSX },          /* 5E */
                                    { "---", PARAM_ERROR },         /* 5F */
                                    { "RTS", PARAM_NONE },          /* 60 */
                                    { "ADC", PARAM_INDIRECTX },     /* 61 */
                                    { "---", PARAM_ERROR },         /* 62 */
                                    { "---", PARAM_ERROR },         /* 63 */
                                    { "---", PARAM_ERROR },         /* 64 */
                                    { "ADC", PARAM_ZPAGE },         /* 65 */
                                    { "ROR", PARAM_ZPAGE },         /* 66 */
                                    { "---", PARAM_ERROR },         /* 67 */
                                    { "PLA", PARAM_NONE },          /* 68 */
                                    { "ADC", PARAM_IMM },           /* 69 */
                                    { "ROR", PARAM_ACC },           /* 6A */
                                    { "---", PARAM_ERROR },         /* 6B */
                                    { "JMP", PARAM_INDIRECTJUMP },  /* 6C */
                                    { "ADC", PARAM_ABSOLUTE },      /* 6D */
                                    { "ROR", PARAM_ABSOLUTE },      /* 6E */
                                    { "---", PARAM_ERROR },         /* 6F */
                                    { "BVS", PARAM_BRANCH },        /* 70 */
                                    { "ADC", PARAM_INDIRECTY },     /* 71 */
                                    { "---", PARAM_ERROR },         /* 72 */
                                    { "---", PARAM_ERROR },         /* 73 */
                                    { "---", PARAM_ERROR },         /* 74 */
                                    { "ADC", PARAM_ZPAGEX },        /* 75 */
                                    { "ROR", PARAM_ZPAGEX },        /* 76 */
                                    { "---", PARAM_ERROR },         /* 77 */
                                    { "SEI", PARAM_NONE },          /* 78 */
                                    { "ADC", PARAM_ABSY },          /* 79 */
                                    { "---", PARAM_ERROR },         /* 7A */
                                    { "---", PARAM_ERROR },         /* 7B */
                                    { "---", PARAM_ERROR },         /* 7C */
                                    { "ADC", PARAM_ABSX },          /* 7D */
                                    { "ROR", PARAM_ABSX },          /* 7E */
                                    { "---", PARAM_ERROR },         /* 7F */
                                    { "---", PARAM_ERROR },         /* 80 */
                                    { "STA", PARAM_INDIRECTX },     /* 81 */
                                    { "---", PARAM_ERROR },         /* 82 */
                                    { "---", PARAM_ERROR },         /* 83 */
                                    { "STY", PARAM_ZPAGE },         /* 84 */
                                    { "STA", PARAM_ZPAGE },         /* 85 */
                                    { "STX", PARAM_ZPAGE },         /* 86 */
                                    { "---", PARAM_ERROR },         /* 87 */
                                    { "DEY", PARAM_NONE },          /* 88 */
                                    { "---", PARAM_ERROR },         /* 89 */
                                    { "TXA", PARAM_NONE },          /* 8A */
                                    { "---", PARAM_ERROR },         /* 8B */
                                    { "STY", PARAM_ABSOLUTE },      /* 8C */
                                    { "STA", PARAM_ABSOLUTE },      /* 8D */
                                    { "STX", PARAM_ABSOLUTE },      /* 8E */
                                    { "---", PARAM_ERROR },         /* 8F */
                                    { "BCC", PARAM_BRANCH },        /* 90 */
                                    { "STA", PARAM_INDIRECTY },     /* 91 */
                                    { "---", PARAM_ERROR },         /* 92 */
                                    { "---", PARAM_ERROR },         /* 93 */
                                    { "STY", PARAM_ZPAGEX },        /* 94 */
                                    { "STA", PARAM_ZPAGEX },        /* 95 */
                                    { "STX", PARAM_ZPAGEY },        /* 96 */
                                    { "---", PARAM_ERROR },         /* 97 */
                                    { "TYA", PARAM_NONE },          /* 98 */
                                    { "STA", PARAM_ABSY },          /* 99 */
                                    { "TXS", PARAM_NONE },          /* 9A */
                                    { "---", PARAM_ERROR },         /* 9B */
                                    { "---", PARAM_ERROR },         /* 9C */
                                    { "STA", PARAM_ABSX },          /* 9D */
                                    { "---", PARAM_ERROR },         /* 9E */
                                    { "---", PARAM_ERROR },         /* 9F */
                                    { "LDY", PARAM_IMM },           /* A0 */
                                    { "LDA", PARAM_INDIRECTX },     /* A1 */
                                    { "LDX", PARAM_IMM },           /* A2 */
                                    { "---", PARAM_ERROR },         /* A3 */
                                    { "LDY", PARAM_ZPAGE },         /* A4 */
                                    { "LDA", PARAM_ZPAGE },         /* A5 */
                                    { "LDX", PARAM_ZPAGE },         /* A6 */
                                    { "---", PARAM_ERROR },         /* A7 */
                                    { "TAY", PARAM_NONE },          /* A8 */
                                    { "LDA", PARAM_IMM },           /* A9 */
                                    { "TAX", PARAM_NONE },          /* AA */
                                    { "---", PARAM_ERROR },         /* AB */
                                    { "LDY", PARAM_ABS },           /* AC */
                                    { "LDA", PARAM_ABS },           /* AD */
                                    { "LDX", PARAM_ABS },           /* AE */
                                    { "---", PARAM_ERROR },         /* AF */
                                    { "BCS", PARAM_BRANCH },        /* B0 */
                                    { "LDA", PARAM_INDIRECTY },     /* B1 */
                                    { "---", PARAM_ERROR },         /* B2 */
                                    { "---", PARAM_ERROR },         /* B3 */
                                    { "LDY", PARAM_ZPAGEX },        /* B4 */
                                    { "LDA", PARAM_ZPAGEX },        /* B5 */
                                    { "LDX", PARAM_ZPAGEY },        /* B6 */
                                    { "---", PARAM_ERROR },         /* B7 */
                                    { "CLV", PARAM_NONE },          /* B8 */
                                    { "LDA", PARAM_ABSY },          /* B9 */
                                    { "TSX", PARAM_NONE },          /* BA */
                                    { "---", PARAM_ERROR },         /* BB */
                                    { "LDY", PARAM_ABSX },          /* BC */
                                    { "LDA", PARAM_ABSX },          /* BD */
                                    { "LDX", PARAM_ABSY },          /* BE */
                                    { "---", PARAM_ERROR },         /* BF */
                                    { "CPY", PARAM_IMM },           /* C0 */
                                    { "CMP", PARAM_INDIRECTX },     /* C1 */
                                    { "---", PARAM_ERROR },         /* C2 */
                                    { "---", PARAM_ERROR },         /* C3 */
                                    { "CPY", PARAM_ZPAGE },         /* C4 */
                                    { "CMP", PARAM_ZPAGE },         /* C5 */
                                    { "DEC", PARAM_ZPAGE },         /* C6 */
                                    { "---", PARAM_ERROR },         /* C7 */
                                    { "INY", PARAM_ERROR },         /* C8 */
                                    { "CMP", PARAM_IMM },           /* C9 */
                                    { "DEX", PARAM_NONE },          /* CA */
                                    { "---", PARAM_ERROR },         /* CB */
                                    { "CPY", PARAM_ABS },           /* CC */
                                    { "CMP", PARAM_ABS },           /* CD */
                                    { "DEC", PARAM_ABS },           /* CE */
                                    { "---", PARAM_ERROR },         /* CF */
                                    { "BNE", PARAM_BRANCH },        /* D0 */
                                    { "CMP", PARAM_INDIRECTY },     /* D1 */
                                    { "---", PARAM_ERROR },         /* D2 */
                                    { "---", PARAM_ERROR },         /* D3 */
                                    { "---", PARAM_ERROR },         /* D4 */
                                    { "CMP", PARAM_ZPAGEX },        /* D5 */
                                    { "DEC", PARAM_ZPAGEX },        /* D6 */
                                    { "---", PARAM_ERROR },         /* D7 */
                                    { "CLD", PARAM_NONE },          /* D8 */
                                    { "CMP", PARAM_ABSY },          /* D9 */
                                    { "---", PARAM_ERROR },         /* DA */
                                    { "---", PARAM_ERROR },         /* DB */
                                    { "---", PARAM_ERROR },         /* DC */
                                    { "CMP", PARAM_ABSX },          /* DD */
                                    { "DEC", PARAM_ABSX },          /* DE */
                                    { "---", PARAM_ERROR },         /* DF */
                                    { "CPX", PARAM_IMM },           /* E0 */
                                    { "SBC", PARAM_INDIRECTX },     /* E1 */
                                    { "---", PARAM_ERROR },         /* E2 */
                                    { "---", PARAM_ERROR },         /* E3 */
                                    { "CPX", PARAM_ZPAGE },         /* E4 */
                                    { "SBC", PARAM_ZPAGE },         /* E5 */
                                    { "INC", PARAM_ZPAGE },         /* E6 */
                                    { "---", PARAM_ERROR },         /* E7 */
                                    { "INX", PARAM_NONE },          /* E8 */
                                    { "SBC", PARAM_IMM },           /* E9 */
                                    { "NOP", PARAM_NONE },          /* EA */
                                    { "---", PARAM_ERROR },         /* EB */
                                    { "CPX", PARAM_ABS },           /* EC */
                                    { "SBC", PARAM_ABS },           /* ED */
                                    { "INC", PARAM_ABS },           /* EE */
                                    { "---", PARAM_ERROR },         /* EF */
                                    { "BEQ", PARAM_BRANCH },        /* F0 */
                                    { "SBC", PARAM_INDIRECTY },     /* F1 */
                                    { "---", PARAM_ERROR },         /* F2 */
                                    { "---", PARAM_ERROR },         /* F3 */
                                    { "---", PARAM_ERROR },         /* F4 */
                                    { "SBC", PARAM_ZPAGEX },        /* F5 */
                                    { "INC", PARAM_ZPAGEX },        /* F6 */
                                    { "---", PARAM_ERROR },         /* F7 */
                                    { "SED", PARAM_NONE },          /* F8 */
                                    { "SBC", PARAM_ABSY },          /* F9 */
                                    { "---", PARAM_ERROR },         /* FA */
                                    { "---", PARAM_ERROR },         /* FB */
                                    { "---", PARAM_ERROR },         /* FC */
                                    { "SBC", PARAM_ABSX },          /* FD */
                                    { "INC", PARAM_ABSX },          /* FE */
                                    { "---", PARAM_ERROR } };       /* FF */

int
getparamsize(char param) {
    switch (param) {
              case PARAM_NONE: return 0;
            case PARAM_BRANCH: return 1;
         case PARAM_INDIRECTX: return 1;
         case PARAM_INDIRECTY: return 1;
             case PARAM_ZPAGE: return 1;
            case PARAM_ZPAGEX: return 1;
            case PARAM_ZPAGEY: return 1;
               case PARAM_IMM: return 1;
               case PARAM_ACC: return 0;
               case PARAM_ABS: return 2;
              case PARAM_ABSX: return 2;
              case PARAM_ABSY: return 2;
           case PARAM_ABSJUMP: return 2;
      case PARAM_INDIRECTJUMP: return 2;
          case PARAM_ABSOLUTE: return 2;
    }
    return 0;
}

void
doparams(int pos,int param) {
    unsigned char c;
    unsigned int  i;

    if (param==PARAM_NONE) return;
    printf("  ");
    c=code[pos];
    i=(int)((char)code[pos])+(int)((char)code[pos+1]*256);

    switch (param) {
            case PARAM_BRANCH: if (c<0x7f) {
                                   printf ("$+%02X",c);
                               } else {
                                   c=0xfe-c;
                                   printf ("$-%02X",c);
                               }
                               return;
         case PARAM_INDIRECTX: printf ("($%02X, X)", c); return;
         case PARAM_INDIRECTY: printf ("($%02X), Y", c); return;
             case PARAM_ZPAGE: printf ("$%02X", c); return;
            case PARAM_ZPAGEX: printf ("$%02X, X", c); return;
            case PARAM_ZPAGEY: printf ("$%02X, Y", c); return;
               case PARAM_IMM: printf ("#$%02X", c); return;
               case PARAM_ACC: printf ("A"); return;
              case PARAM_ABSX: c=code[pos+1];
                               printf ("$%02X", c);
                               c=code[pos];
                               printf ("%02X, X", c);
                               return;
              case PARAM_ABSY: c=code[pos+1];
                               printf ("$%02X", c);
                               c=code[pos];
                               printf ("%02X, Y", c);
                               return;
           case PARAM_ABSJUMP: c=code[pos+1];
                               printf ("$%02X", c);
                               c=code[pos];
                               printf ("%02X", c);
                               return;
      case PARAM_INDIRECTJUMP: c=code[pos+1];
                               printf ("($%02X", c);
                               c=code[pos];
                               printf ("%02X)", c);
                               return;
          case PARAM_ABSOLUTE:
               case PARAM_ABS: c=code[pos+1];
                               printf ("$%02X", c);
                               c=code[pos];
                               printf ("%02X", c);
                               return;
    }
}

void
main() {
    char tempstr[TEMPSTR_LEN];
    FILE *f;
    unsigned int  i,c,j,c2,s;
    unsigned char arg;

    printf ("6510 Disassembler Version 1.0 - (c) 1999 Rink Springer - PD\n\n");
    printf ("File to disassemble: ");
    gets (tempstr);
    printf ("Base offset (in memory): $");
    scanf("%x",&base_offs);
    printf ("Starting offset (in file): $");
    scanf("%x",&start_offs);
    printf ("Size: $");
    scanf("%x",&bytes_to_disasm);

    if ((f=fopen(tempstr,"rb"))==NULL) {
        printf ("Unable to open '%s'\n",tempstr);
        exit(1);
    }
    fseek(f,0,SEEK_END); code_size=ftell(f); rewind(f);
    if ((code=(char*)malloc(code_size))==NULL) {
        printf ("Unable to allocate %lu bytes of memory\n",code_size);
        exit(1);
    }
    if(!fread(code,code_size,1,f)) {
        printf("Unable to read file\n");
        exit(1);
    }
    fclose(f);

    if (!bytes_to_disasm) bytes_to_disasm=code_size-start_offs;
    if ((bytes_to_disasm+start_offs) > code_size) {
        printf ("Cannot disassemble more bytes than program size");
        exit(1);
    }

    for(i=start_offs;i<(start_offs+bytes_to_disasm);) {
        c=(unsigned char)code[i];
        printf ("%04X:",i+base_offs);
        arg=instr[c].params;
        s=getparamsize(arg)+1;
        for(j=0;j<s;j++) {
            c2=code[i+j];
            printf (" %02X",(unsigned char)c2);
        }
        for (j=0;j<(12-(s*3));j++) {
            printf(" ");
        }
        printf(" %s", instr[c].name);

        doparams(i+1,arg);
        i+=s;

        printf("\n");
   }
   free(code);
}
