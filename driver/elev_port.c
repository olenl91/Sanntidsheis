#include "elev.h"
#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>

typedef uint8_t byte;

int read_cmd(byte *buf);
int write_cmd(byte *buf, int len);
int read_exact(byte *buf, int len);
int write_exact(byte *buf, int len);

void poll_button_signals();
void poll_floor_sensors();

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
  byte buf[100];


  while (1) {

      if(read_cmd(buf) > 0){

	  switch(buf[0]){
	  case(1):
	      elev_init(buf[1]);
	      break;
	  case(2):
	      elev_set_motor_direction(buf[1]);
	      break;
	  case(3):
	      elev_set_door_open_lamp(buf[1]);
	      break;
	  case(4):
	      buf[0] = elev_get_obstruction_signal();
	      write_cmd(buf, 1);
	      break;
	  case(5):
	      buf[0] = elev_get_stop_signal();
	      write_cmd(buf, 1);
	      break;
	  case(6):
	      elev_set_stop_lamp(buf[1]);
	      break;
	  case(7):
	      buf[0] = elev_get_floor_sensor_signal();
	      write_cmd(buf, 1);
	      break;
	  case(8):
	      elev_set_floor_indicator(buf[1]);
	      break;
	      /* case(9): */
	      /* 	  res = elev_get_button_signal(buf[1], buf[2]); */
	      /* 	  break; */
	  case(10):
	      elev_set_button_lamp(buf[1], buf[2], buf[3]);
	      break;	  
	  case(11):
	      poll_button_signals();
	      break;
	  case(12):
	      poll_floor_sensors();
		  break;
	  case(13):
		  exit(0);

	  }
      }
      

  }

  return 0;
}


void poll_floor_sensors()
{
    byte buf[100];

    static int previousFloor;

    int newFloor = elev_get_floor_sensor_signal();

    buf[0] = 12;

    if(newFloor != -1 && newFloor != previousFloor){
	buf[1] = newFloor;
	write_cmd(buf, 2);
    }
    
    previousFloor = newFloor;

}


void poll_button_signals()
{
    static int previousUp[N_FLOORS];
    static int previousDown[N_FLOORS];
    static int previousCommand[N_FLOORS];
    
    byte buf[100];



    for(int i = 0; i < N_FLOORS; i++) // poller masse, fix
    {
	buf[0] = 11;

	if(i != N_FLOORS-1){
	    int up = elev_get_button_signal(BUTTON_CALL_UP, i);
	    if(up && !previousUp[i]){
		buf[1] = 0;
		buf[2] = i;
		write_cmd(buf, 3);
	    }
	    previousUp[i] = up;
	}
	
	if(i != 0){
	    int down = elev_get_button_signal(BUTTON_CALL_DOWN, i);
	    if(down && !previousDown[i]){
		buf[1] = 1;
		buf[2] = i;
		write_cmd(buf, 3);
	    }
	    previousDown[i] = down;
	}
	
	int command = elev_get_button_signal(BUTTON_COMMAND, i);
	if(command && previousCommand[i]){
	    buf[1] = 2;
	    buf[2] = i;
	    write_cmd(buf, 3);
	}
	previousCommand[i] = command;
      }
   
}
