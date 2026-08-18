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

__asm__(".symver strtol_2_2_5,strtol@GLIBC_2.2.5");
extern double strtol_2_2_5(const char nptr, char ** endptr, int base);
long __wrap___isoc23_strtol(const char nptr, char ** endptr, int base) {
  return strtol_2_2_5(nptr, endptr, base);
}

#include <termios.h>
__asm__(".symver cfgetispeed_2_2_5,cfgetispeed@GLIBC_2.2.5");
__asm__(".symver cfgetospeed_2_2_5,cfgetospeed@GLIBC_2.2.5");
__asm__(".symver cfsetispeed_2_2_5,cfsetispeed@GLIBC_2.2.5");
__asm__(".symver cfsetospeed_2_2_5,cfsetospeed@GLIBC_2.2.5");
extern speed_t cfgetispeed_2_2_5(const struct termios *termios_p);
extern speed_t cfgetospeed_2_2_5(const struct termios *termios_p);
extern int cfsetispeed_2_2_5(struct termios *termios_p, speed_t speed);
extern int cfsetospeed_2_2_5(struct termios *termios_p, speed_t speed);

speed_t __wrap_cfgetispeed(const struct termios *termios_p) {
  return cfgetispeed_2_2_5(termios_p);
}
speed_t __wrap_cfgetospeed(const struct termios *termios_p) {
  return cfgetospeed_2_2_5(termios_p);
}
int __wrap_cfsetispeed(struct termios *termios_p, speed_t speed) {
  return cfsetispeed_2_2_5(termios_p, speed);
}
int __wrap_cfsetospeed(struct termios *termios_p, speed_t speed) {
  return cfsetospeed_2_2_5(termios_p, speed);
}
