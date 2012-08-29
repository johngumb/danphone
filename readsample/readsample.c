#include <stdio.h>

int main(int argc, char *argv[])
{
    int a;
    int s;
    int i=0;

    while (1)
    {
        short val;
        int nb;

        i++;

        nb=fread(&val,2,2,stdin);

        if (nb>0)
        {
            if ((i%10000)==0)
                printf("%X \n",val);
        }
        else
            printf("%d nb\n",nb);
    }
}
