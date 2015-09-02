// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifndef _WIN32
#include <unistd.h>
#include <sys/ioctl.h>
#endif
#include <fcntl.h>
#include <errno.h>
#ifdef __linux__
#include <signal.h>
#endif
#include <readline/readline.h>
#include <readline/history.h>
#include <curses.h>
#include <sys/time.h>
#define READ 0
#define WRITE 1

// gcc c_src/cmdshell.c -I/usr/local/Cellar/readline/6.3.8/include/ /usr/local/Cellar/readline/6.3.8/lib/libreadline.a -lncurses  -o cmdshell

char running=1;
int infp, outfp;
int req = 0, resp = 0;
const char *prompt = "actordb> ";
char pipe_req[128];
char pipe_resp[128];
unsigned long long int mytime = 0;

pid_t popen2(char* argv[], int argc, int *infp, int *outfp)
{
	int p_stdin[2], p_stdout[2];
	pid_t pid;

	if (pipe(p_stdin) != 0 || pipe(p_stdout) != 0)
		return -1;

	pid = fork();
	if (pid < 0)
		return pid;

	else if (pid == 0)
	{
		char *args[argc];
		int i;
		args[0] = argv[1];
		// args[1] = "pipe";
		// args[2] = pipe_req;
		// args[3] = pipe_resp;
		for (i = 2; i < argc; i++)
			args[i-2+1] = argv[i];
		args[i-2+1] = NULL;

		close(p_stdin[WRITE]);
		dup2(p_stdin[READ], READ);
		close(p_stdout[READ]);
		dup2(p_stdout[WRITE], WRITE);
		// execl("/bin/sh", "sh", "-c", command, NULL);
		execvp(argv[1],args);
		exit(1);
	}

	if (infp == NULL)
		close(p_stdin[WRITE]);
	else
		*infp = p_stdin[WRITE];
	if (outfp == NULL)
		close(p_stdout[READ]);
	else
		*outfp = p_stdout[READ];
	return pid;
}

void proc_exit()
{
	running = 0;
}

static void rl_handler(char* line)
{
	if (line == NULL)
	{
		running = 0;
		return;
	}
	if (strlen(line) > 1)
		add_history(line);

	// #ifdef TIOCGWINSZ
	// {
	// 	struct winsize ws;
	// 	if (ioctl(0, TIOCGWINSZ, &ws) >= 0)
	// 	{
	// 		char dim[30];
	// 		sprintf(dim,"dim=%d,%d\n",(int)ws.ws_row, (int)ws.ws_col),
	// 		write(req, dim, strlen(dim));
	// 	}
	// }
	// #endif
	if (write(req, line, strlen(line)) < 0)
	{
		running = 0;
	}
}

void stop()
{
	running = 0;
	close(STDIN_FILENO);
	close(resp);
}

int main(int argc, char *argv[])
{
	signal(SIGCHLD, proc_exit);
	signal(SIGQUIT, stop);
	signal(SIGINT, stop);
	int nread = 0, sread = 0;
	char buf[1024*64];
	struct timeval tv;
	gettimeofday(&tv, NULL);
	FILE *comfile = NULL;

	if (argc < 2)
	{
		printf("Missing program to execute\n");
		return 0;
	}

	mytime = tv.tv_sec*1000 + (tv.tv_usec / 1000);
	snprintf(pipe_req,125,"/tmp/actordb.%llu.req",mytime);
	snprintf(pipe_resp,125,"/tmp/actordb.%llu.resp",mytime);

	comfile = fopen("/tmp/comfile","wb");
	sprintf(buf,"%s\n%s",pipe_req,pipe_resp);
	fwrite(buf,strlen(buf),1,comfile);
	fclose(comfile);

	mknod(pipe_req, S_IFIFO|0666, 0);
	mknod(pipe_resp, S_IFIFO|0666, 0);

	resp = open(pipe_resp,O_RDONLY | O_NONBLOCK);
	if (resp == -1)
	{
		printf("Unable to open resp pipe\n");
		goto finished;
	}
	if (popen2(argv, argc, &infp, &outfp) <= 0)
	{
		printf("Unable to exec your-program-B\n");
		goto finished;
	}
	

	req = open(pipe_req,O_WRONLY);
	if (req == -1)
	{
		printf("Unable to open req pipe\n");
		goto finished;
	}
	rl_callback_handler_install(prompt, &rl_handler);

	while (running)
	{
		int rc;
		fd_set fdread;
		FD_ZERO(&fdread);
		FD_SET(STDIN_FILENO, &fdread);
		FD_SET(resp, &fdread);

		rc = select(resp+1, &fdread, NULL, NULL, NULL);

		if (FD_ISSET(STDIN_FILENO, &fdread))
		{
			rl_callback_read_char();
		}
		else if (FD_ISSET(resp, &fdread))
		{
			int offset = 0;
			sread = read(resp, buf, sizeof(buf)-1);
			if (sread <= 0 && errno == EWOULDBLOCK)
				continue;
			else if (sread <= 0)
			{
				rl_set_prompt("");
				rl_redisplay();
				break;
			}
			buf[sread] = 0;
			if (buf[0] == '~' && buf[1] == '~')
			{
				int i = 0;
				for (i = 0; i < sread; i++)
				{
					if (buf[i] == '\n' || buf[i] == '\r')
					{
						buf[i] = 0;
						break;
					}
				}
				if (strcmp(buf+2,"getpass") == 0)
				{
					char *line = NULL;

					rl_save_prompt();
					rl_replace_line("",0);
					rl_redisplay();
					line = getpass("Password:");
					write(req, line, strlen(line));
					rl_clear_message();
					rl_restore_prompt();
					rl_redisplay();
					continue;
				}
				else
				{
					rl_set_prompt(buf+2);
					rl_redisplay();
				}

				for (; i < sread; i++)
				{
					if (buf[i] >= '!')
						break;
				}
				if (i == sread)
					continue;

				offset = i;
				sread = strlen(buf+offset);
			}

			rl_save_prompt();
			rl_replace_line("",0);
			rl_redisplay();
			printf("%.*s",sread,buf+offset);
			rl_clear_message();
			rl_restore_prompt();
			rl_redisplay();
			// printlog(buf,sread);
		}
	}
	rl_callback_handler_remove();

finished:
	unlink(pipe_req);
	unlink(pipe_resp);

	return 0;
}
