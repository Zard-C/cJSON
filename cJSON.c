/*
   Copyright (c) 2009 Dave Gamble

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
   THE SOFTWARE.
   */

/* cJSON */
/* JSON parser in C. */

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <float.h>
#include <limits.h>
#include <ctype.h>
#include "cJSON.h"

/* error infomation  */ 
static const char *ep;

/* 获取错误信息 */
const char *cJSON_GetErrorPtr(void) {return ep;}

static int cJSON_strcasecmp(const char *s1,const char *s2)
{
    if (!s1) return (s1==s2)?0:1;if (!s2) return 1;
    for(; tolower(*s1) == tolower(*s2); ++s1, ++s2)	if(*s1 == 0)	return 0;
    return tolower(*(const unsigned char *)s1) - tolower(*(const unsigned char *)s2);
}

/* 函数指针，指向malloc函数，可以使用typedef*/
static void *(*cJSON_malloc)(size_t sz) = malloc;
/* 函数指针，指向了free函数，也可以使用typedef*/
static void (*cJSON_free)(void *ptr) = free;

/* strdup的实现，strdup是标准库函数，此处返回值加了static，与标准库不同, 表示该函数不作为对外接口，仅在本文件内使用*/
static char* cJSON_strdup(const char* str)
{
    size_t len;
    char* copy;

    len = strlen(str) + 1;
    if (!(copy = (char*)cJSON_malloc(len))) return 0;
    memcpy(copy,str,len);
    return copy;
}

/* 初始化入口, 外部接口，如果没有提供对应的函数则使用默认malloc, free */
void cJSON_InitHooks(cJSON_Hooks* hooks)
{
    /* 如果传入了nullptr  */
    if (!hooks) 
    { 
        /* 使用默认的malloc free */
        /* Reset hooks */
        cJSON_malloc = malloc;
        cJSON_free = free;
        return;
    }

    /* 否则 使用用户提供的malloc_fn 和 free_fn */
    cJSON_malloc = (hooks->malloc_fn)?hooks->malloc_fn:malloc;
    cJSON_free	 = (hooks->free_fn)?hooks->free_fn:free;
}

/* 内部构造函数，申请空间并初始化，相当于new */
/* Internal constructor. */
static cJSON *cJSON_New_Item(void)
{
    cJSON* node = (cJSON*)cJSON_malloc(sizeof(cJSON));
    if (node) memset(node,0,sizeof(cJSON));
    return node;
}

/* cJSON对象 的销毁操作 */
/* Delete a cJSON structure. */
void cJSON_Delete(cJSON *c)
{
    cJSON *next;
    while (c)
    {
        next=c->next;
        if (!(c->type&cJSON_IsReference) && c->child) cJSON_Delete(c->child);			/* 孩子节点的销毁 */
        if (!(c->type&cJSON_IsReference) && c->valuestring) cJSON_free(c->valuestring);		/* 释放valuestring */
        if (!(c->type&cJSON_StringIsConst) && c->string) cJSON_free(c->string);				/* 释放 string */
        cJSON_free(c);	/* 释放当前节点指向的元素 */
        c=next;
    }
}

/* 数字解析: 解析输入文本并将生成的数字填入到item中 */
/* Parse the input text to generate a number, and populate the result into item. */
static const char *parse_number(cJSON *item, const char *num)
{
    double n=0, sign=1, scale=0;				/*n 表示结果，sign 表示正负号, 默认为正，scale 表示比例*/
    int subscale=0,signsubscale=1;

    if (*num=='-') sign=-1,num++;	/* Has sign? */
    if (*num=='0') num++;			/* is zero */
    if (*num>='1' && *num<='9')	do	n=(n*10.0)+(*num++ -'0');	while (*num>='0' && *num<='9');	/* Number? */
    if (*num=='.' && num[1]>='0' && num[1]<='9') /* 小数部分 */
    {
        num++;		do	n=(n*10.0)+(*num++ -'0'),scale--; while (*num>='0' && *num<='9');
    }	/* Fractional part? */
    if (*num=='e' || *num=='E')		/* Exponent? */
    {	
        num++;if (*num=='+') num++;	else if (*num=='-') signsubscale=-1,num++;		/* With sign? */
        while (*num>='0' && *num<='9') subscale = (subscale*10) + (*num++ - '0');	/* Number? */
    }

    n=sign*n*pow(10.0, (scale + subscale*signsubscale));	/* number = +/- number.fraction * 10^+/- exponent */

    /* 将计算的结果填充到item */
    item->valuedouble = n;
    item->valueint = (int)n;
    item->type = cJSON_Number;
    return num; /* 返回输入的const char* */
}

/* 动态扩容: 返回不小于x的最小的2的N次方。eg. 10 ->16 */
static int pow2gt (int x)	{	--x;	x|=x>>1;	x|=x>>2;	x|=x>>4;	x|=x>>8;	x|=x>>16;	return x+1;	}

typedef struct {char *buffer; int length; int offset; } printbuffer; /* 手动扩容buffer */

/* 辅助函数: 检查缓冲区的大小是否满足填充长度 */
static char* ensure(printbuffer *p,int needed)
{
    char *newbuffer;
    int newsize;
    if (!p || !p->buffer) return 0;
    needed+=p->offset;  /* 计算需要的偏移量 */
    if (needed<=p->length) return p->buffer+p->offset;

    /* 如果偏移量大于buffer当前的容量 */
    newsize=pow2gt(needed); /* 加倍策略 */
    /* 实现动态扩容, 深拷贝 */
    newbuffer=(char*)cJSON_malloc(newsize);
    if (!newbuffer) {cJSON_free(p->buffer);p->length=0,p->buffer=0;return 0;}
    if (newbuffer) memcpy(newbuffer,p->buffer,p->length);
    cJSON_free(p->buffer);
    p->length=newsize;
    p->buffer=newbuffer;
    return newbuffer + p->offset;
}

/* 更新当前偏移位置 */
static int update(printbuffer *p)
{
    char *str;
    if (!p || !p->buffer) return 0;
    str=p->buffer+p->offset;
    return p->offset+strlen(str);
}

/* 将给定项目中的数字转换为字符串 */
/* Render the number nicely from the given item into a string. */
static char *print_number(cJSON *item, printbuffer *p)
{
    char *str=0;
    double d=item->valuedouble;
    if (d==0)
    {
        if (p)	str=ensure(p,2);
        else	str=(char*)cJSON_malloc(2);	/* special case for 0. */
        if (str) strcpy(str,"0");
    }
    else if (fabs(((double)item->valueint)-d)<=DBL_EPSILON && d<=INT_MAX && d>=INT_MIN) /* 精度满足整数表示法，使用整数表示 */
    {
        if (p)	str=ensure(p,21);
        else	str=(char*)cJSON_malloc(21);	/* 2^64+1 can be represented in 21 chars. */
        if (str)	sprintf(str,"%d",item->valueint);
    }
    else
    {
        if (p)	str=ensure(p,64);
        else	str=(char*)cJSON_malloc(64);	/* This is a nice tradeoff. */
        if (str)
        {
            if (fabs(floor(d)-d)<=DBL_EPSILON && fabs(d)<1.0e60)sprintf(str,"%.0f",d);      /*  float or double 显示小数点后0 位 */
            else if (fabs(d)<1.0e-6 || fabs(d)>1.0e9)			sprintf(str,"%e",d);        /* 数值过大或者过小，使用e表示法 */
            else												sprintf(str,"%f",d);        /* float  */
        }
    }
    return str;
}

/* 辅助函数: unt16 -> utf-8 编码转换 */
static unsigned parse_hex4(const char *str)
{
    unsigned h=0;
    if (*str>='0' && *str<='9') h+=(*str)-'0'; else if (*str>='A' && *str<='F') h+=10+(*str)-'A'; else if (*str>='a' && *str<='f') h+=10+(*str)-'a'; else return 0;
    h=h<<4;str++;
    if (*str>='0' && *str<='9') h+=(*str)-'0'; else if (*str>='A' && *str<='F') h+=10+(*str)-'A'; else if (*str>='a' && *str<='f') h+=10+(*str)-'a'; else return 0;
    h=h<<4;str++;
    if (*str>='0' && *str<='9') h+=(*str)-'0'; else if (*str>='A' && *str<='F') h+=10+(*str)-'A'; else if (*str>='a' && *str<='f') h+=10+(*str)-'a'; else return 0;
    h=h<<4;str++;
    if (*str>='0' && *str<='9') h+=(*str)-'0'; else if (*str>='A' && *str<='F') h+=10+(*str)-'A'; else if (*str>='a' && *str<='f') h+=10+(*str)-'a'; else return 0;
    return h;
}

/* Parse the input text into an unescaped cstring, and populate item. */
static const unsigned char firstByteMark[7] = { 0x00, 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC };


/* 字符串解析: 将输入文本转换为转义c风格字符串并赋给 item结构体valuestring 成员 */
static const char *parse_string(cJSON *item, const char *str)
{
    const char *ptr=str+1;char *ptr2;char *out;int len=0;
    unsigned uc,uc2;

    if (*str!='\"') {ep=str;return 0;}	/* not a string! */ /* 不是以 " 开头，不是字符串 */

    while (*ptr!='\"' && *ptr && ++len) if (*ptr++ == '\\') ptr++;	/* Skip escaped quotes. */ /* 跳过转义字符 */

    out=(char*)cJSON_malloc(len+1);	/* This is how long we need for the string, roughly. */
    if (!out) return 0;

    ptr=str+1;ptr2=out;
    while (*ptr!='\"' && *ptr)
    {
        if (*ptr!='\\') *ptr2++=*ptr++;
        else
        {
            ptr++;
            switch (*ptr)
            {
            case 'b': *ptr2++='\b';	break;
            case 'f': *ptr2++='\f';	break;
            case 'n': *ptr2++='\n';	break;
            case 'r': *ptr2++='\r';	break;
            case 't': *ptr2++='\t';	break;
            case 'u':	 /* transcode utf16 to utf8. */ /* 将utf16编码转换为 utf-8 编码 */
                      uc=parse_hex4(ptr+1);ptr+=4;	/* get the unicode char. */

                      if ((uc>=0xDC00 && uc<=0xDFFF) || uc==0)	break;	/* check for invalid.	*/

                      if (uc>=0xD800 && uc<=0xDBFF)	/* UTF16 surrogate pairs.	*/
                      {
                          if (ptr[1]!='\\' || ptr[2]!='u')	break;	/* missing second-half of surrogate.	*/
                          uc2=parse_hex4(ptr+3);ptr+=6;
                          if (uc2<0xDC00 || uc2>0xDFFF)		break;	/* invalid second-half of surrogate.	*/
                          uc=0x10000 + (((uc&0x3FF)<<10) | (uc2&0x3FF));
                      }

                      len=4;if (uc<0x80) len=1;else if (uc<0x800) len=2;else if (uc<0x10000) len=3; ptr2+=len;

                      switch (len) {
                      case 4: *--ptr2 =((uc | 0x80) & 0xBF); uc >>= 6;
                      case 3: *--ptr2 =((uc | 0x80) & 0xBF); uc >>= 6;
                      case 2: *--ptr2 =((uc | 0x80) & 0xBF); uc >>= 6;
                      case 1: *--ptr2 =(uc | firstByteMark[len]);
                      }
                      ptr2+=len;
                      break;
            default:  *ptr2++=*ptr; break;
            }
            ptr++;
        }
    }
    *ptr2=0;
    if (*ptr=='\"') ptr++;
    item->valuestring=out;          /* 生成的字符串指针由valuestring 管理 */
    item->type=cJSON_String;        /* String 保存在valuestring 成员中 */
    return ptr;
}

/* 字符串打印: 将提供的 cstring 呈现给可以打印的转义版本 */
/* Render the cstring provided to an escaped version that can be printed.  */
static char *print_string_ptr(const char *str, printbuffer *p)
{
    const char *ptr;
    char *ptr2,*out;
    int len=0,flag = 0; /* flag 表示是否含有转义字符 */
    unsigned char token; /* 占位符 */

    /* 遍历并查看是否有转义字符 */
    for (ptr=str; *ptr; ptr++) 
        flag|=((*ptr>0 && *ptr<32)||(*ptr=='\"')||(*ptr=='\\')) ? 1:0;


    if (!flag)
    {
        len=ptr-str;                                /* 字符串长度 */
        if (p) out = ensure(p,len+3);               /* 保证空间，3个字符是是加前后引号和结尾\0 */
        else		out=(char*)cJSON_malloc(len+3);
        if (!out) return 0;
        ptr2=out;
        *ptr2++='\"';       /* ptr2[0] = '"' */
        strcpy(ptr2,str);
        ptr2[len]='\"';
        ptr2[len+1]=0;
        return out;
    }

    if (!str)   /* 空字符串展示为 "" */
    {
        if (p)	out=ensure(p,3);
        else	out=(char*)cJSON_malloc(3);
        if (!out) return 0;
        strcpy(out,"\"\"");
        return out;
    }

    /* 转义字符的处理，留出足够的长度 */
    ptr=str;
    while ((token=*ptr) && ++len) 
    {
        if (strchr("\"\\\b\f\n\r\t",token)) 
            len++; 
        else if (token<32) 
            len+=5;
        ptr++;
    }

    if (p)	out=ensure(p,len+3);
    else	out=(char*)cJSON_malloc(len+3);
    if (!out) return 0;

    ptr2=out;
    ptr=str;
    *ptr2++='\"';

    while (*ptr)
    {
        if ((unsigned char)*ptr>31 && *ptr!='\"' && *ptr!='\\') *ptr2++=*ptr++;
        else /* 转义字符的处理 将 '\b' 一个字符拆成 '\\' + 'b' */
        {
            *ptr2++='\\';
            switch (token=*ptr++)
            {
            case '\\':	*ptr2++='\\';	break;
            case '\"':	*ptr2++='\"';	break;
            case '\b':	*ptr2++='b';	break;
            case '\f':	*ptr2++='f';	break;
            case '\n':	*ptr2++='n';	break;
            case '\r':	*ptr2++='r';	break;
            case '\t':	*ptr2++='t';	break;
            default: sprintf(ptr2,"u%04x",token);ptr2+=5;	break;	/* escape and print */
            }
        }
    }

    *ptr2++='\"'; /* 字符串结尾" */
    *ptr2++=0;    /* 末尾0 */
    return out; 
}

/* Invote print_string_ptr (which is useful) on an item. */
static char *print_string(cJSON *item,printbuffer *p)	{return print_string_ptr(item->valuestring,p);}

/* Predeclare these prototypes. */
static const char *parse_value(cJSON *item,const char *value);
static char *print_value(cJSON *item,int depth,int fmt,printbuffer *p);
static const char *parse_array(cJSON *item,const char *value);
static char *print_array(cJSON *item,int depth,int fmt,printbuffer *p);
static const char *parse_object(cJSON *item,const char *value);
static char *print_object(cJSON *item,int depth,int fmt,printbuffer *p);

/* 跳过空格 cr  or lf (ascii <= 32 的字符)  */
/* Utility to jump whitespace and cr/lf */
static const char *skip(const char *in) 
{
    while (in && *in && (unsigned char)*in<=32) 
        in++; 
    return in;
}

/* 对象解析过程 - 创建新的root节点并填充 */
/* Parse an object - create a new root, and populate. */
cJSON *cJSON_ParseWithOpts(const char *value, const char **return_parse_end, int require_null_terminated)
{
    const char *end = 0;            /*解析结果 */
    cJSON *c = cJSON_New_Item();
    ep = 0;
    if (!c) return 0;       /* memory fail */

    end = parse_value(c,skip(value));
    if (!end)	{cJSON_Delete(c);return 0;}	/* parse failure. ep is set. */

    /* if we require null-terminated JSON without appended garbage, skip and then check for a null terminator */
    if (require_null_terminated) 
    {
        end=skip(end);
        if (*end) 
        {
            cJSON_Delete(c);ep=end;return 0;
        }
    }
    if (return_parse_end) *return_parse_end=end;
    return c;
}

/* 按照默认选项解析 */
/* Default options for cJSON_Parse */
cJSON *cJSON_Parse(const char *value) {return cJSON_ParseWithOpts(value,0,0);}

/* 将 cJSON 项目/实体/结构呈现为文本 */
/* Render a cJSON item/entity/structure to text. */
char *cJSON_Print(cJSON *item)				{return print_value(item, 0, 1, 0);}
char *cJSON_PrintUnformatted(cJSON *item)	{return print_value(item, 0, 0, 0);}

char *cJSON_PrintBuffered(cJSON *item,int prebuffer,int fmt)
{
    printbuffer p;
    p.buffer=(char*)cJSON_malloc(prebuffer);
    p.length=prebuffer;
    p.offset=0;
    return print_value(item,0,fmt,&p);
    return p.buffer;
}

/* 解析核心处理部分 */
/* Parser core - when encountering text, process appropriately. */
static const char *parse_value(cJSON *item, const char *value)
{
    if (!value)						return 0;	/* Fail on null. */
    if (!strncmp(value,"null",4))	{ item->type=cJSON_NULL;  return value+4; }                 
    if (!strncmp(value,"false",5))	{ item->type=cJSON_False; return value+5; }
    if (!strncmp(value,"true",4))	{ item->type=cJSON_True; item->valueint=1;	return value+4; }
    if (*value=='\"')				{ return parse_string(item,value); }                            /* 解析字符串 eg. "", "abc"*/
    if (*value=='-' || (*value>='0' && *value<='9'))	{ return parse_number(item,value); }        /* 解析数字 eg. 1, 3.14, 1.0e+1 */
    if (*value=='[')				{ return parse_array(item,value); }                             /* 解析array eg. [1, 2, 3, 4, 5] */
    if (*value=='{')				{ return parse_object(item,value); }                            /* 解析对象 eg. { "name" = "bill", "name" = "gates" }  */

    ep=value;   /* 非法的输入 */
    return 0;	/* failure. */
}

/* 用文本方式展示item 的各项值  */
/* Render a value to text. */
static char *print_value(cJSON *item, int depth, int fmt, printbuffer *p)
{
    char *out=0;
    if (!item) return 0;

    /* 提供了打印缓冲区 */
    if (p)
    {
        switch ((item->type)&255)
        {
        case cJSON_NULL:	{out=ensure(p,5);	if (out) strcpy(out,"null");	break;}
        case cJSON_False:	{out=ensure(p,6);	if (out) strcpy(out,"false");	break;}
        case cJSON_True:	{out=ensure(p,5);	if (out) strcpy(out,"true");	break;}
        case cJSON_Number:	out=print_number(item,p);break;
        case cJSON_String:	out=print_string(item,p);break;
        case cJSON_Array:	out=print_array(item,depth,fmt,p);break;
        case cJSON_Object:	out=print_object(item,depth,fmt,p);break;
        }
    }
    else /* 未提供缓冲区 */
    {
        switch ((item->type)&255)
        {
        case cJSON_NULL:	out=cJSON_strdup("null");	break;
        case cJSON_False:	out=cJSON_strdup("false");break;
        case cJSON_True:	out=cJSON_strdup("true"); break;
        case cJSON_Number:	out=print_number(item,0);break;
        case cJSON_String:	out=print_string(item,0);break;
        case cJSON_Array:	out=print_array(item,depth,fmt,0);break;
        case cJSON_Object:	out=print_object(item,depth,fmt,0);break;
        }
    }
    return out;
}

/* 从输入文本构建一个数组 */
/* Build an array from input text. */
static const char *parse_array(cJSON *item, const char *value)
{
    cJSON *child;
    if (*value!='[')	{ep=value;return 0;}	/* not an array! */

    item->type=cJSON_Array;

    value=skip(value+1);
    if (*value==']') return value+1;	/* empty array. */ /* 空数组，提前退出 返回value + 1 */

    /* 处理array中的第一个元素 */
    item->child=child=cJSON_New_Item();
    if (!item->child) return 0;		 /* memory fail */

    value= skip(parse_value(child, skip(value)));	/* skip any spacing, get the value. */
    if (!value) return 0;

    /* array中剩余元素通过 prev 和 next 链接 */
    while (*value==',')
    {
        cJSON *new_item;
        if (!(new_item=cJSON_New_Item())) return 0; 	/* memory fail */
        child->next=new_item; new_item->prev=child; child=new_item;
        value=skip(parse_value(child,skip(value+1)));
        if (!value) return 0;	/* memory fail */
    }

    if (*value==']') return value+1;	/* end of array */

    /* 解析失败返回 0， 并且将ep设置为当前出错的字符串 */
    ep=value; 
    return 0;	/* malformed. */
}



/* 用文本方式展示数组 */
/* Render an array to text */
static char *print_array(cJSON *item, int depth, int fmt, printbuffer *p)
{
    char **entries;
    char *out=0,*ptr,*ret;
    int len=5;                  /* 空数组 " [ ] " \0 对应长度为5 */ 
    cJSON *child=item->child;
    int numentries=0,i=0,fail=0;    /* numentries 数组大小 */
    size_t tmplen=0;

    /* 统计数组中元素的个数 */
    /* How many entries in the array? */
    while (child) numentries++,child=child->next;

    /* 显式处理空数组的情况 */
    /* Explicitly handle numentries==0 */
    if (!numentries)
    {
        /* 保证空间足够 */
        if (p)	out=ensure(p,3);
        else	out=(char*)cJSON_malloc(3);

        if (out) strcpy(out,"[]");
        return out;
    }

    if (p) 
    {

        /* 构建输出 */
        /* Compose the output array. */
        i=p->offset;
        ptr=ensure(p,1);if (!ptr) return 0;	*ptr='[';	p->offset++;
        child=item->child;
        while (child && !fail)
        {
            /* 打印数组元素的值 */
            print_value(child,depth+1,fmt,p);
            /* 更新buffer容量信息 */
            p->offset=update(p);
            /* fmt 用于格式控制 fmt == 0: a,b   fmt == 1:  a, b 多了一个空格 */
            if (child->next) 
            {
                len=fmt?2:1;
                ptr=ensure(p,len+1);
                if (!ptr) 
                    return 0;
                *ptr++=',';
                if(fmt)
                    *ptr++=' ';
                *ptr=0;
                p->offset+=len;
            }
            child=child->next;
        }

        /* 保证结尾能够容纳 ] \0 */
        ptr=ensure(p,2);if (!ptr) return 0;	*ptr++=']';*ptr=0;

        /* 给出字符串在buffer中的位置 */
        out=(p->buffer)+i; 
    }
    else /* 没有提供buffer结构体 */
    {
        
        /* 分配char*类型指针数组空间 */
        /* Allocate an array to hold the values for each */
        entries=(char**)cJSON_malloc(numentries*sizeof(char*));
        if (!entries) return 0;
        memset(entries,0,numentries*sizeof(char*));

        /* Retrieve all the results: */
        child=item->child;
        while (child && !fail)
        {
            ret=print_value(child,depth+1,fmt,0);
            entries[i++]=ret;
            if (ret) len+=strlen(ret)+2+(fmt?1:0); else fail=1;
            child=child->next;
        }

        /* If we didn't fail, try to malloc the output string */
        if (!fail)	out=(char*)cJSON_malloc(len);
        /* If that fails, we fail. */
        if (!out) fail=1;

        /* 失败处理: 遍历entries数组，并将已经申请的内存释放 */
        /* Handle failure. */
        if (fail)
        {
            for (i=0;i<numentries;i++) if (entries[i]) cJSON_free(entries[i]);
            cJSON_free(entries);
            return 0;
        }

        /* 组成输出数组 */
        /* Compose the output array. */
        *out='[';                    /* out[0] = '[' */

        ptr=out+1;*ptr=0;
        for (i=0;i<numentries;i++)
        {
            tmplen=strlen(entries[i]);
            memcpy(ptr,entries[i],tmplen); /* 将entries[i] ->child[i]中的内容写入到输出数组中 */
            ptr+=tmplen;
            if (i!=numentries-1) 
            {
                *ptr++=',';
                if(fmt)*ptr++=' ';
                *ptr=0;
            }
            cJSON_free(entries[i]);     /* (char*)指向的空间内容已经被复制，可以释放了 */
        }
        cJSON_free(entries);            /* 最后将entries 指针（char**）空间释放 */
        *ptr++=']';*ptr++=0;
    }
    return out;	
}

/* 对象解析: 从文本构建一个对象 */
/* Build an object from the text. */
static const char *parse_object(cJSON *item, const char *value)
{
    cJSON *child;
    if (*value!='{')	{ep=value;return 0;}	/* not an object! */

    item->type=cJSON_Object;                    
    value=skip(value+1);
    if (*value=='}') return value+1;	/* empty object. */

    item->child=child=cJSON_New_Item(); /* 非空对象，申请空间并继续向下解析 */
    if (!item->child) return 0;

    /* 在 JSON 中，键必须是字符串，由双引号包围：   {"name": "Bill Gates"}  */
    value=skip(parse_string(child,skip(value)));
    if (!value) return 0;

    /* 将解析出保存在valuestring 中的字符串 作为key, 由string管理，然后解析: 后的部分作为value  */
    child->string=child->valuestring;child->valuestring=0; 
    if (*value!=':') {ep=value;return 0;}	/* fail! */

    /* parse_value 递归调用 */
    value=skip(parse_value(child, skip(value+1)));	/* skip any spacing, get the value. */
    if (!value) return 0;

    while (*value==',') /* 多个值之间使用 , 进行分割  */
    {
        cJSON *new_item;  /* 解析下一个对象 */
        if (!(new_item=cJSON_New_Item()))	return 0; /* memory fail */
        child->next=new_item;new_item->prev=child;child=new_item;

        /* 解析key */
        value=skip(parse_string(child,skip(value+1)));
        if (!value) return 0;

        /* 转移管理权给string  */
        child->string=child->valuestring;child->valuestring=0;
        if (*value!=':') {ep=value;return 0;}	/* fail! */

        /* 解析值，值可以是字符串，数字，对象，数组, null，得到valuestring */
        value=skip(parse_value(child,skip(value+1)));	/* skip any spacing, get the value. */
        if (!value) return 0;
    }

    if (*value=='}') return value+1;	/* end of array */ /* 读取完整个对象的信息，返回字符}的下一个位置 */
    ep=value;return 0;	/* malformed. */
}

/* 对象显示: 以文本方式显示对象 */
/* Render an object to text. */
static char *print_object(cJSON *item,int depth,int fmt,printbuffer *p)
{
    char **entries=0,**names=0;
    char *out=0,*ptr,*ret,*str;int len=7,i=0,j;
    cJSON *child=item->child;
    int numentries=0,fail=0;
    size_t tmplen=0;

    /* 统计元素个数 */
    /* Count the number of entries. */
    while (child) numentries++,child=child->next;

    /* 显示处理空元素的情况 */
    /* Explicitly handle empty object case */
    if (!numentries)
    {
        if (p) out=ensure(p,fmt?depth+4:3);
        else	out=(char*)cJSON_malloc(fmt?depth+4:3);
        if (!out)	return 0;
        ptr=out;*ptr++='{';
        if (fmt) {*ptr++='\n';for (i=0;i<depth-1;i++) *ptr++='\t';}
        *ptr++='}';*ptr++=0;
        return out;
    }

    /* 如果提供了存放字符串的空间 */
    if (p)
    {
        /* Compose the output: */
        i=p->offset;
        len=fmt?2:1;	ptr=ensure(p,len+1);	if (!ptr) return 0;
        *ptr++='{';	if (fmt) *ptr++='\n';	*ptr=0;	p->offset+=len;
        child=item->child;depth++;
        
        while (child)
        {
            if (fmt)
            {
                ptr=ensure(p,depth);	if (!ptr) return 0;
                for (j=0;j<depth;j++) *ptr++='\t';
                p->offset+=depth;
            }
            print_string_ptr(child->string,p);
            p->offset=update(p);

            len=fmt?2:1;
            ptr=ensure(p,len);	if (!ptr) return 0;
            *ptr++=':';if (fmt) *ptr++='\t';
            p->offset+=len;

            print_value(child,depth,fmt,p);
            p->offset=update(p);

            len=(fmt?1:0)+(child->next?1:0);
            ptr=ensure(p,len+1); if (!ptr) return 0;
            if (child->next) *ptr++=',';
            if (fmt) *ptr++='\n';*ptr=0;
            p->offset+=len;
            child=child->next;
        }
        ptr=ensure(p,fmt?(depth+1):2);	 if (!ptr) return 0;
        if (fmt)	for (i=0;i<depth-1;i++) *ptr++='\t';
        *ptr++='}';*ptr=0;
        out=(p->buffer)+i;
    }
    else
    {
        /* Allocate space for the names and the objects */
        entries=(char**)cJSON_malloc(numentries*sizeof(char*));
        if (!entries) return 0;
        names=(char**)cJSON_malloc(numentries*sizeof(char*));
        if (!names) {cJSON_free(entries);return 0;}
        memset(entries,0,sizeof(char*)*numentries);
        memset(names,0,sizeof(char*)*numentries);

        /* Collect all the results into our arrays: */
        child=item->child;depth++;if (fmt) len+=depth;
        while (child)
        {
            names[i]=str=print_string_ptr(child->string,0);
            entries[i++]=ret=print_value(child,depth,fmt,0);
            if (str && ret) len+=strlen(ret)+strlen(str)+2+(fmt?2+depth:0); else fail=1;
            child=child->next;
        }

        /* Try to allocate the output string */
        if (!fail)	out=(char*)cJSON_malloc(len);
        if (!out) fail=1;

        /* Handle failure */
        if (fail)
        {
            for (i=0;i<numentries;i++) {if (names[i]) cJSON_free(names[i]);if (entries[i]) cJSON_free(entries[i]);}
            cJSON_free(names);cJSON_free(entries);
            return 0;
        }

        /* Compose the output: */
        *out='{';ptr=out+1;if (fmt)*ptr++='\n';*ptr=0;
        for (i=0;i<numentries;i++)
        {
            if (fmt) for (j=0;j<depth;j++) *ptr++='\t';
            tmplen=strlen(names[i]);memcpy(ptr,names[i],tmplen);ptr+=tmplen;
            *ptr++=':';if (fmt) *ptr++='\t';
            strcpy(ptr,entries[i]);ptr+=strlen(entries[i]);
            if (i!=numentries-1) *ptr++=',';
            if (fmt) *ptr++='\n';*ptr=0;
            cJSON_free(names[i]);cJSON_free(entries[i]);
        }

        cJSON_free(names);cJSON_free(entries);
        if (fmt) for (i=0;i<depth-1;i++) *ptr++='\t';
        *ptr++='}';*ptr++=0;
    }
    return out;	
}

/* Get Array size/item / object item. */
/* 获取数组容量 */
int    cJSON_GetArraySize(cJSON *array)							{cJSON *c=array->child;int i=0;while(c)i++,c=c->next;return i;}
/* 获取数组中item个元素的地址 */
cJSON *cJSON_GetArrayItem(cJSON *array,int item)				{cJSON *c=array->child;  while (c && item>0) item--,c=c->next; return c;}
/* 获取对象中key 为 string 的元素 */
cJSON *cJSON_GetObjectItem(cJSON *object,const char *string)	{cJSON *c=object->child; while (c && cJSON_strcasecmp(c->string,string)) c=c->next; return c;}

/* Utility for array list handling. */
/* 处理链表尾部插入 */
static void suffix_object(cJSON *prev,cJSON *item) {prev->next=item;item->prev=prev;}

/* Utility for handling references. */
/* 创建引用 */
static cJSON *create_reference(cJSON *item) 
{
    cJSON *ref=cJSON_New_Item();
    if (!ref) return 0;
    memcpy(ref,item,sizeof(cJSON));
    ref->string=0;
    ref->type|=cJSON_IsReference;
    ref->next=ref->prev=0;
    return ref;
}

/* Add item to array/object. */
/* 向对象 或者 数组中添加项目 */
void   cJSON_AddItemToArray(cJSON *array, cJSON *item)						
{
    cJSON *c=array->child;if (!item) return; 
    if (!c) {array->child=item;} 
    else {while (c && c->next) c=c->next; suffix_object(c,item);}
}
void   cJSON_AddItemToObject(cJSON *object,const char *string,cJSON *item)	{if (!item) return; if (item->string) cJSON_free(item->string);item->string=cJSON_strdup(string);cJSON_AddItemToArray(object,item);}
void   cJSON_AddItemToObjectCS(cJSON *object,const char *string,cJSON *item)	{if (!item) return; if (!(item->type&cJSON_StringIsConst) && item->string) cJSON_free(item->string);item->string=(char*)string;item->type|=cJSON_StringIsConst;cJSON_AddItemToArray(object,item);}
void	cJSON_AddItemReferenceToArray(cJSON *array, cJSON *item)						{cJSON_AddItemToArray(array,create_reference(item));}
void	cJSON_AddItemReferenceToObject(cJSON *object,const char *string,cJSON *item)	{cJSON_AddItemToObject(object,string,create_reference(item));}

/* 从数组中分离某个元素，下标由which指定 */
cJSON *cJSON_DetachItemFromArray(cJSON *array,int which)			
{
    cJSON *c=array->child;while (c && which>0) c=c->next,which--;if (!c) return 0;
    if (c->prev) c->prev->next=c->next;if (c->next) c->next->prev=c->prev;if (c==array->child) array->child=c->next;c->prev=c->next=0;return c;
}
/* 从数组中删除某个元素，下标由which指定*/
void   cJSON_DeleteItemFromArray(cJSON *array,int which)			
{
    cJSON_Delete(cJSON_DetachItemFromArray(array,which));
}

/* 从对象中分离某个项目，由string 指定 */
cJSON *cJSON_DetachItemFromObject(cJSON *object,const char *string) 
{
    int i=0;cJSON *c=object->child;while (c && cJSON_strcasecmp(c->string,string)) i++,c=c->next;if (c) return cJSON_DetachItemFromArray(object,i);return 0;
}

/* 从对象中删除某个项目， 由string指定*/
void   cJSON_DeleteItemFromObject(cJSON *object,const char *string) 
{
    cJSON_Delete(cJSON_DetachItemFromObject(object,string));
}


/* Replace array/object items with new ones. */
/* 向数组中插入元素 */
void   cJSON_InsertItemInArray(cJSON *array,int which,cJSON *newitem)		
{
    cJSON *c=array->child;
    while (c && which>0) c=c->next,which--;
    
    /* array为空 */
    if (!c) 
    {
        cJSON_AddItemToArray(array,newitem);return;
    }

    /* array不为空 */
    newitem->next=c;newitem->prev=c->prev;c->prev=newitem;
    if (c==array->child) array->child=newitem; 
    else newitem->prev->next=newitem;
}

/* 替换数组中的元素 */
void   cJSON_ReplaceItemInArray(cJSON *array,int which,cJSON *newitem)		
{
    cJSON *c=array->child;while (c && which>0) c=c->next,which--;if (!c) return;
    newitem->next=c->next;newitem->prev=c->prev;if (newitem->next) newitem->next->prev=newitem;
    
    if (c==array->child) array->child=newitem; 
    else newitem->prev->next=newitem;c->next=c->prev=0;
    cJSON_Delete(c); /* 释放被替换掉的内容 */
}

/* 替换对象中的元素 */
void   cJSON_ReplaceItemInObject(cJSON *object,const char *string,cJSON *newitem)
{
    int i=0;cJSON *c=object->child;
    while(c && cJSON_strcasecmp(c->string,string))i++,c=c->next;
    if(c)
    {
        newitem->string=cJSON_strdup(string);
        cJSON_ReplaceItemInArray(object,i,newitem);
    }
}


/* Create basic types: */
/* 创建基本类型 */
cJSON *cJSON_CreateNull(void)					{cJSON *item=cJSON_New_Item();if(item)item->type=cJSON_NULL;return item;}
cJSON *cJSON_CreateTrue(void)					{cJSON *item=cJSON_New_Item();if(item)item->type=cJSON_True;return item;}
cJSON *cJSON_CreateFalse(void)					{cJSON *item=cJSON_New_Item();if(item)item->type=cJSON_False;return item;}
cJSON *cJSON_CreateBool(int b)					{cJSON *item=cJSON_New_Item();if(item)item->type=b?cJSON_True:cJSON_False;return item;}
cJSON *cJSON_CreateNumber(double num)			{cJSON *item=cJSON_New_Item();if(item){item->type=cJSON_Number;item->valuedouble=num;item->valueint=(int)num;}return item;}
cJSON *cJSON_CreateString(const char *string)	{cJSON *item=cJSON_New_Item();if(item){item->type=cJSON_String;item->valuestring=cJSON_strdup(string);}return item;}
cJSON *cJSON_CreateArray(void)					{cJSON *item=cJSON_New_Item();if(item)item->type=cJSON_Array;return item;}
cJSON *cJSON_CreateObject(void)					{cJSON *item=cJSON_New_Item();if(item)item->type=cJSON_Object;return item;}

/* Create Arrays: */
/* 创建数组 */
cJSON *cJSON_CreateIntArray(const int *numbers,int count)		{int i;cJSON *n=0,*p=0,*a=cJSON_CreateArray();for(i=0;a && i<count;i++){n=cJSON_CreateNumber(numbers[i]);if(!i)a->child=n;else suffix_object(p,n);p=n;}return a;}
cJSON *cJSON_CreateFloatArray(const float *numbers,int count)	{int i;cJSON *n=0,*p=0,*a=cJSON_CreateArray();for(i=0;a && i<count;i++){n=cJSON_CreateNumber(numbers[i]);if(!i)a->child=n;else suffix_object(p,n);p=n;}return a;}
cJSON *cJSON_CreateDoubleArray(const double *numbers,int count)	{int i;cJSON *n=0,*p=0,*a=cJSON_CreateArray();for(i=0;a && i<count;i++){n=cJSON_CreateNumber(numbers[i]);if(!i)a->child=n;else suffix_object(p,n);p=n;}return a;}
cJSON *cJSON_CreateStringArray(const char **strings,int count)	{int i;cJSON *n=0,*p=0,*a=cJSON_CreateArray();for(i=0;a && i<count;i++){n=cJSON_CreateString(strings[i]);if(!i)a->child=n;else suffix_object(p,n);p=n;}return a;}

/* Duplication */
/* 对象的拷贝动作：调用时根据item的child 是否为空设置 recurse */
cJSON *cJSON_Duplicate(cJSON *item, int recurse)
{
    cJSON *newitem,*cptr,*nptr=0,*newchild;
    /* Bail on bad ptr */
    if (!item) return 0;
    /* Create new item */
    newitem=cJSON_New_Item();
    if (!newitem) return 0;

    /* 深拷贝动作 */
    /* Copy over all vars */

    /* ？引用是做什么用的 ? */
    newitem->type=item->type&(~cJSON_IsReference),newitem->valueint=item->valueint,newitem->valuedouble=item->valuedouble;
    
    
    if (item->valuestring)	{newitem->valuestring=cJSON_strdup(item->valuestring);	if (!newitem->valuestring)	{cJSON_Delete(newitem);return 0;}}
    if (item->string)		{newitem->string=cJSON_strdup(item->string);			if (!newitem->string)		{cJSON_Delete(newitem);return 0;}}
    
    /* child 为空，不需要在进行拷贝了 */
    /* If non-recursive, then we're done! */
    if (!recurse) return newitem;

    /* 将child指向的array中内容拷贝 */
    /* Walk the ->next chain for the child. */
    cptr=item->child;
    while (cptr)
    {
        newchild=cJSON_Duplicate(cptr,1);		/* Duplicate (with recurse) each item in the ->next chain */ 
        if (!newchild) {cJSON_Delete(newitem);return 0;}
        if (nptr)	{nptr->next=newchild,newchild->prev=nptr;nptr=newchild;}	/* If newitem->child already set, then crosswire ->prev and ->next and move on */
        else		{newitem->child=newchild;nptr=newchild;}					/* Set newitem->child and move to it */ /* 刚插入时 nptr为空 */
        cptr=cptr->next;
    }
    return newitem;
}

/* json最小化： */
void cJSON_Minify(char *json)
{
    char *into=json;
    while (*json)
    {
        if (*json==' ') json++;
        else if (*json=='\t') json++;	/* Whitespace characters. */
        else if (*json=='\r') json++;
        else if (*json=='\n') json++;
        else if (*json=='/' && json[1]=='/')  while (*json && *json!='\n') json++;	/* double-slash comments, to end of line. */
        else if (*json=='/' && json[1]=='*') {while (*json && !(*json=='*' && json[1]=='/')) json++;json+=2;}	/* multiline comments. */
        else if (*json=='\"'){*into++=*json++;while (*json && *json!='\"'){if (*json=='\\') *into++=*json++;*into++=*json++;}*into++=*json++;} /* string literals, which are \" sensitive. */
        else *into++=*json++;			/* All other characters. */
    }
    *into=0;	/* and null-terminate. */
}
