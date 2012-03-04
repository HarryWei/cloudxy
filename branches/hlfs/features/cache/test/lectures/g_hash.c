/*
 *  test/lectures/g_hash.c
 *
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 */
#include <glib.h>

int main(int argc, char **argv) {
	int i = 0;

	//g_direct_hash <convert a gpointer to a hash value> -- g_direct_equal
	//g_int_hash <convert a pointer to a gint to a hash value>-- g_int_equal
	//g_str_hash <convert a string to a hash value> -- g_str_equal
	GHashTable *hash = g_hash_table_new(g_direct_hash, g_direct_equal);
	//GHashTable *g_hash_table_new_full (GHashFunc hash_func,
	//									GHashFunc key_equal_func,
	//									GDestroyNotify key_destroy_func,
	//									GDestroyNotify value_destroy_func)
	for (i = 0; i < 10; i++) {
		g_hash_table_insert(hash, GINT_TO_POINTER(i), GINT_TO_POINTER (i + 1));
	}
	g_message("There are %d keys in the hash", g_hash_table_size(hash));
	g_message("Key 4's value is %d",  GPOINTER_TO_INT (g_hash_table_lookup(hash, GINT_TO_POINTER (4))));
	gboolean found = g_hash_table_remove(hash, GINT_TO_POINTER (5));
	g_message("The value of 5 was %sfound and removed", found ? "" : "not ");
	g_hash_table_destroy(hash);
	return 0;
}
