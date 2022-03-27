/** @file ctcss-jack.c
 *
 * @brief simple sine tone generator
 */

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include <jack/jack.h>
#include <assert.h>
#include <ctype.h>

jack_port_t *output_port;
jack_client_t *client;

#ifndef M_PI
#define M_PI  (3.14159265)
#endif

// 32,000/77.0Hz = 415, 800 should be enough
#define MAX_TABLE_SIZE 800

typedef struct
{
    float sine[MAX_TABLE_SIZE];
    int phase;
    int tablesize;
    float freq;
}
paTestData;

static void signal_handler(int sig)
{
	jack_client_close(client);
	fprintf(stderr, "signal received, exiting ...\n");
	exit(0);
}

/**
 * The process callback for this JACK application is called in a
 * special realtime thread once for each audio cycle.
 *
 * This client follows a simple rule: when the JACK transport is
 * running, copy the input port to the output.  When it stops, exit.
 */

int
process (jack_nframes_t nframes, void *arg)
{
	jack_default_audio_sample_t *out;
	paTestData *data = (paTestData*)arg;
	int i;

	out = (jack_default_audio_sample_t*)jack_port_get_buffer (output_port, nframes);

	for( i=0; i<nframes; i++ )
    {
        out[i] = data->sine[data->phase];  /* left */
        data->phase += 1;
        if( data->phase >= data->tablesize ) data->phase -= data->tablesize;
    }
    
	return 0;      
}

/*
 * JACK calls this shutdown_callback if the server ever shuts down or
 * decides to disconnect the client.
 */
void
jack_shutdown (void *arg)
{
	exit (1);
}

static int srate(jack_nframes_t nframes, void *arg)
{
	paTestData *data = (paTestData*)arg;
	printf("the sample rate is now %" PRIu32 "/sec\n", nframes);

    data->tablesize = (nframes*10)/(int)(data->freq*10);
    assert(data->tablesize<MAX_TABLE_SIZE);

	for(int i=0; i<data->tablesize; i++ )
    {
        data->sine[i] = 0.5 * (float) sin( ((double)i/(double)data->tablesize) * M_PI * 2. );
    }
    data->phase = 0;

	return 0;
}

int
main (int argc, char *argv[])
{
	const char **ports;
	const char *client_name;
	const char *server_name = NULL;
	jack_options_t options = JackNullOption;
	jack_status_t status;
	paTestData data;
	int c;
    float floatfreq=77.0;
    char servername[256]={0};
    char clientname[256]={0};

    while ((c = getopt (argc, argv, "c:f:s:")) != -1)
    {
        switch (c)
        {
        case 'f':
            floatfreq = atof(optarg);
            break;
        case 's':
            strcpy(servername, optarg);
            break;
        case 'c':
            strcpy(clientname, optarg);
            break;
        case '?':
            switch (optopt)
            {
            case 'c':
            case 'f':
            case 's':
                fprintf (stderr, "Option -%c requires an argument.\n", optopt);
            default:
                return 2;
            }
        default:
            abort ();
        }
    }

	if (strlen(clientname)) {		/* client name specified? */
		client_name = clientname;
		if (strlen(servername)) {	/* server name specified? */
			server_name = servername;
            int my_option = JackNullOption | JackServerName;
			options = (jack_options_t)my_option;
		}
	} else {			/* use basename of argv[0] */
		client_name = strrchr(argv[0], '/');
		if (client_name == 0) {
			client_name = argv[0];
		} else {
			client_name++;
		}
	}

    data.freq = floatfreq;

	/* open a client connection to the JACK server */

	client = jack_client_open (client_name, options, &status, server_name);
	if (client == NULL) {
		fprintf (stderr, "jack_client_open() failed, "
			 "status = 0x%2.0x\n", status);
		if (status & JackServerFailed) {
			fprintf (stderr, "Unable to connect to JACK server\n");
		}
		exit (1);
	}
	if (status & JackServerStarted) {
		fprintf (stderr, "JACK server started\n");
	}
	if (status & JackNameNotUnique) {
		client_name = jack_get_client_name(client);
		fprintf (stderr, "unique name `%s' assigned\n", client_name);
	}

 	jack_set_sample_rate_callback (client, srate, &data);

	/* tell the JACK server to call `process()' whenever
	   there is work to be done.
	*/

	jack_set_process_callback (client, process, &data);

	/* tell the JACK server to call `jack_shutdown()' if
	   it ever shuts down, either entirely, or if it
	   just decides to stop calling us.
	*/

	jack_on_shutdown (client, jack_shutdown, 0);

	/* create port */

	output_port = jack_port_register (client, "output",
					  JACK_DEFAULT_AUDIO_TYPE,
					  JackPortIsOutput, 0);

	if (output_port == NULL) {
		fprintf(stderr, "no more JACK ports available\n");
		exit (1);
	}

	/* Tell the JACK server that we are ready to roll.  Our
	 * process() callback will start running now. */

	if (jack_activate (client)) {
		fprintf (stderr, "cannot activate client");
		exit (1);
	}

	/* Connect the ports.  You can't do this before the client is
	 * activated, because we can't make connections to clients
	 * that aren't running.  Note the confusing (but necessary)
	 * orientation of the driver backend ports: playback ports are
	 * "input" to the backend, and capture ports are "output" from
	 * it.
	 */
 	
	ports = jack_get_ports (client, NULL, NULL,
				JackPortIsPhysical|JackPortIsInput);
	if (ports == NULL) {
		fprintf(stderr, "no physical playback ports\n");
		exit (1);
	}

 #if 0
	if (jack_connect (client, jack_port_name (output_port1), ports[0])) {
		fprintf (stderr, "cannot connect output ports\n");
	}

	if (jack_connect (client, jack_port_name (output_port2), ports[1])) {
		fprintf (stderr, "cannot connect output ports\n");
	}
#endif

	free (ports);
    
    /* install a signal handler to properly quits jack client */
#ifdef WIN32
	signal(SIGINT, signal_handler);
    signal(SIGABRT, signal_handler);
	signal(SIGTERM, signal_handler);
#else
	signal(SIGQUIT, signal_handler);
	signal(SIGTERM, signal_handler);
	signal(SIGHUP, signal_handler);
	signal(SIGINT, signal_handler);
#endif

	/* keep running until the Ctrl+C */

	while (1) {
	#ifdef WIN32 
		Sleep(1000);
	#else
		sleep (1);
	#endif
	}

	jack_client_close (client);
	exit (0);
}
