#include "alloc.h"
#include "driver.h"

int main(int argc, char **argv) {
    FuHeapList_init(FU_TRUE);

    int res = FuDriver_main(argc, argv);

    FuHeapList_drop();
    return res;
}
