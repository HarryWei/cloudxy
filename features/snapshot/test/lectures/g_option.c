/*
 * lectures/g_option.c
 *
 * Harry Wei <harryxiyou@gmail.com> (C) 2011
 */
#include <glib.h>
#include <stdlib.h>
#if 0
typedef struct {
	const gchar *long_name;
	gchar short_name;
	gint flags;
	GOptionArg arg;
	gpointer arg_data;
	const gchar *description;
	const gchar *arg_description;
} GOptionEntry;
#endif

static gchar *fsname = NULL;
static gchar *fsuse = NULL;
static gint fssize = 0;
static gboolean verbose = FALSE;

static GOptionEntry entries[] = {
	{"fsname", 'n', 0, G_OPTION_ARG_STRING, &fsname, "file system name", "FSNAME"},
	{"fsuse", 'u', 0, G_OPTION_ARG_STRING, &fsuse, "file system use", "FSUSE"},
	{"fssize", 's', 0, G_OPTION_ARG_INT, &fssize, "file system size(B)", "FSSIZE"},
	{"verbose", 'v', 0, G_OPTION_ARG_NONE, &verbose, "Be verbose", NULL},
	{NULL}
};

int main(int argc, char *argv[])
{
	GError *error = NULL;
	GOptionContext *context = NULL;
	context = g_option_context_new("- test option");
	g_option_context_add_main_entries(context, entries, NULL);
	g_option_context_set_help_enabled(context, TRUE);
	g_option_context_set_summary(context, "test option");
	if (!g_option_context_parse(context, &argc, &argv, &error)) {
		g_print("option parsing failed: %s\n", error->message);
		exit(1);
	}
	g_option_context_free(context);
	g_print("fsname is %s\n", fsname);
	g_print("fsuse is %s\n", fsuse);
	g_print("fssize is %d\n", fssize);
	return 0;
}
