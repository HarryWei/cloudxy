/*
 *  lectures/g_list.c
 *
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 */
#include <glib.h>

struct num {
	int id;
	int a;
};

static gint
sort(gconstpointer p1, gconstpointer p2) {
	int a = 0;
	int b = 0;

	a = ((struct num *) p1)->a;
	b = ((struct num *) p2)->a;
	g_print("a is %d\n", a);
	g_print("b is %d\n", b);
#if 1
	if (a < b) {
		return 1;
	} else if (a == b) {
		return 0;
	} else {
		return -1;
	}
#endif
//	return (a < b ? +1 : a == b ? 0 : -1);
}

static gint
sort_r(gconstpointer p1, gconstpointer p2) {
	int a = 0;
	int b = 0;

//	a = GPOINTER_TO_INT (p1);
//	b = GPOINTER_TO_INT (p2);
	a = ((struct num *) p1)->a;
	b = ((struct num *) p2)->a;
	if (a > b) {
		return 1;
	} else if (a == b) {
		return 0;
	} else {
		return -1;
	}
}

static void
print(gpointer p1, gpointer p2) {
//	g_print("%d,", *(int *)p1);
	g_print("%d,", ((struct num *) p1)->a); 
	return ;
}

int main(int argc, char **argv) {
	int i = 0;
	GList *list = NULL;

	struct num nums[10] = {
		{1, 10},
		{2, 8},
		{3, 9},
		{4, 1},
		{5, 3},
		{6, 4},
		{7, 5},
		{8, 2},
		{9, 6},
		{10, 7}
	};
//	memset(nums, 0, sizeof(struct num) * 10);
//	gint nums[10] = {10, 8, 9, 1, 3, 4, 5, 2, 6, 7};

	for (i = 0; i < 10; i++) {
		list = g_list_append(list, &nums[i]);
	}
	list = g_list_sort(list, sort);
	g_list_foreach(list, print, NULL);
	g_print("\n");
#if 1
	list = g_list_sort(list, sort_r);
	g_list_foreach(list, print, NULL);
#endif
	g_print("\n");
	return 0;
}
