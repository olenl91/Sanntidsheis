#include "elev.h"
#include <unistd.h>
#include <stdint.h>

typedef uint8_t byte;

int read_cmd(byte *buf);
int write_cmd(byte *buf, int len);
int read_exact(byte *buf, int len);
int write_exact(byte *buf, int len);


int read_cmd(byte *buf)
{
  int len;

  if (read_exact(buf, 2) != 2)
    return(-1);
  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, len);
}

int write_cmd(byte *buf, int len)
{
  byte li;

  li = (len >> 8) & 0xff;
  write_exact(&li, 1);
  
  li = len & 0xff;
  write_exact(&li, 1);

  return write_exact(buf, len);
}

int read_exact(byte *buf, int len)
{
  int i, got=0;

  do {
    if ((i = read(0, buf+got, len-got)) <= 0)
      return(i);
    got += i;
  } while (got<len);

  return(len);
}

int write_exact(byte *buf, int len)
{
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);

  return (len);
}


int main() 
{
  int res;
  byte buf[100];

  while (read_cmd(buf) > 0) {

    switch(buf[0]){
    case(1):
      elev_init(buf[1]);
      res = 0;
      break;
    case(2):
      elev_set_motor_direction(buf[1]);
      res = buf[1];
      break;
    case(3):
      elev_set_door_open_lamp(buf[1]);
      res = 0;
      break;
    case(4):
      res = elev_get_obstruction_signal();
      break;
    case(5):
      res = elev_get_stop_signal();
      break;
    case(6):
      elev_set_stop_lamp(buf[1]);
      res = 0;
      break;
    case(7):
      res = elev_get_floor_sensor_signal();
      break;
    case(8):
      elev_set_floor_indicator(buf[1]);
      res = 0;
      break;
    case(9):
      res = elev_get_button_signal(buf[1], buf[2]);
      break;
    case(10):
      elev_set_button_lamp(buf[1], buf[2], buf[3]);
      res = 0;
      break;

    }

    buf[0] = res;
    write_cmd(buf, 1);
  }

  return 0;
}
