#include <stdbool.h>
#include <string.h>

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "erl_interface.h"
#include "ei.h"

#include "elev.h"

#define BUFFERSIZE 100

#define HOST_NAME "127.0.0.1"
#define NODE_NAME "heis"
#define FULL_NAME "heis@127.0.0.1" // sett inn correct IP
#define COOKIE "secretcookie"
#define IP "127.0.0.1"
#define CONNECT_TO_NODE "master@127.0.0.1"



int main()
{
	struct in_addr addr;
	int fd; // grant this a more reasonable name some fitting time
	uint8_t receiveBuffer[BUFFERSIZE];

	addr.s_addr = inet_addr(IP);

	erl_init(NULL, 0);
	if(!erl_connect_xinit(HOST_NAME, NODE_NAME, FULL_NAME, &addr, COOKIE, 0))
		erl_err_quit("<ERROR> when initializing !");

	fd = erl_connect(CONNECT_TO_NODE);


	while(true){
		ErlMessage erlangMessage;
		printf("here3\n\r");
		int receiveResult = erl_receive_msg(fd, receiveBuffer, BUFFERSIZE, &erlangMessage);
		printf("here4\n\r");

		printf("moo\n\r");

		switch(receiveResult){
		case ERL_TICK:
			break;
		case ERL_ERROR:
			break;
		default:
			if(erlangMessage.type == ERL_REG_SEND){
				ETERM * funAtom = erl_element(1, erlangMessage.msg);
				
				
				if(!strncmp(ERL_ATOM_PTR(funAtom), "elev_init", 9)){
					ETERM * interfaceAtom = erl_element(2, erlangMessage.msg);
					if(!strncmp(ERL_ATOM_PTR(interfaceAtom), "simulator", 9)){
						elev_init(1); // fix this ugly hack
					} else if(!strncmp(ERL_ATOM_PTR(interfaceAtom), "simulator", 9)){
						elev_init(2); // fix this ugly hack
					}
					erl_free_term(interfaceAtom);
									

				} else if(!strncmp(ERL_ATOM_PTR(funAtom), "elev_set_motor_direction", 24)){
					ETERM * directionAtom = erl_element(2, erlangMessage.msg);
					if(!strncmp(ERL_ATOM_PTR(directionAtom), "up", 2))
						elev_set_motor_direction(DIRN_UP);
					if(!strncmp(ERL_ATOM_PTR(directionAtom), "down", 4))
						elev_set_motor_direction(DIRN_DOWN);
					if(!strncmp(ERL_ATOM_PTR(directionAtom), "stop", 4))
						elev_set_motor_direction(DIRN_STOP);
					erl_free_term(directionAtom);
					

				} else if(!strncmp(ERL_ATOM_PTR(funAtom), "elev_set_door_open_lamp", 23)) {
					ETERM * statusAtom = erl_element(2, erlangMessage.msg);
					if(!strncmp(ERL_ATOM_PTR(statusAtom), "on", 2))
						elev_set_door_open_lamp(1);
					else if (!strncmp(ERL_ATOM_PTR(statusAtom), "off", 3))
						elev_set_door_open_lamp(0);
					erl_free_term(statusAtom);
					

				} else if(!strncmp(ERL_ATOM_PTR(funAtom), "elev_set_stop_lamp", 18)) {
					ETERM * statusAtom = erl_element(2, erlangMessage.msg);
					if(!strncmp(ERL_ATOM_PTR(statusAtom), "on", 2))
						elev_set_stop_lamp(1);
					else if (!strncmp(ERL_ATOM_PTR(statusAtom), "off", 3))
						elev_set_stop_lamp(0);
					erl_free_term(statusAtom);
					

				} else if(!strncmp(ERL_ATOM_PTR(funAtom), "elev_set_floor_indicator", 24)) {
					ETERM * floor = erl_element(2, erlangMessage.msg);
					elev_set_floor_indicator(ERL_INT_VALUE(floor));
					erl_free_term(floor);
					

				} else if(!strncmp(ERL_ATOM_PTR(funAtom), "elev_set_button_lamp", 20)) {
					ETERM * buttonAtom = erl_element(2, erlangMessage.msg);
					ETERM * floor = erl_element(3, erlangMessage.msg);
					ETERM * statusAtom = erl_element(4, erlangMessage.msg);
					int buttonOn;
					
					if(!strncmp(ERL_ATOM_PTR(statusAtom), "on", 2))
						buttonOn = 1;
					if(!strncmp(ERL_ATOM_PTR(statusAtom), "off", 3))
						buttonOn = 0;
					
					
					if(!strncmp(ERL_ATOM_PTR(buttonAtom), "up", 2))
						elev_set_button_lamp(0, ERL_INT_VALUE(floor), buttonOn);
					else if (!strncmp(ERL_ATOM_PTR(buttonAtom), "dowm", 4))
						elev_set_button_lamp(1, ERL_INT_VALUE(floor), buttonOn);
					else if (!strncmp(ERL_ATOM_PTR(buttonAtom), "command", 7))
						elev_set_button_lamp(2, ERL_INT_VALUE(floor), buttonOn);
					
					erl_free_term(floor);
					erl_free_term(statusAtom);

				}

				erl_free_term(erlangMessage.from);
				erl_free_term(erlangMessage.msg);
				erl_free_term(funAtom);


			
				

			}

		}
		
	}


}
