// My glibc is ahead of debian-stable, so the exe fails to start when containerized with
// the debian-stable image. The proper fix would be at least one of:
//
// - to build in the container, to ensure consistency
//
// - static linking with musl libc + jemalloc (because musc libc malloc seems
// significantly worse than glibc malloc, even with the 2021-ish reimplementation)

#include <math.h>
__asm__(".symver fmod_2_2_5,fmod@GLIBC_2.2.5");
extern double fmod_2_2_5(double x, double y);
double __wrap_fmod(double x, double y) {
  return fmod_2_2_5(x, y);
}
