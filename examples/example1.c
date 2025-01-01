/* ---------------------------------------------------------------------
 * Demonstration program for PGPLOT called from C [Convex UNIX].
 *----------------------------------------------------------------------
 */

MAIN__()
{
    int i;
    static float xs[] = {1.0, 2.0, 3.0, 4.0, 5.0 };
    static float ys[] = {1.0, 4.0, 9.0, 16.0, 25.0 };
    float xr[100], yr[100];
    long dummy, nx, ny;
    float xmin, xmax, ymin, ymax;
    long just, axis, n, symbol;
/*
 * Call PGBEGIN to initiate PGPLOT and open the output device; PGBEGIN
 * will prompt the user to supply the device name and type.
 */
    dummy = 0;
    nx = 1;
    ny = 1;
    pgbegin_(&dummy, "?", &nx, &ny, 1L);
/*
 * Call PGENV to specify the range of the axes and to draw a box, and
 * PGLABEL to label it. The x-axis runs from 0 to 10, and y from 0 to 20.
 */
    xmin = 0.0;
    xmax = 10.0;
    ymin = 0.0;
    ymax = 20.0;
    just = 0;
    axis = 1;
    pgenv_(&xmin, &xmax, &ymin, &ymax, &just, &axis);
    pglabel_("(x)", "(y)", "PGPLOT Example 1 - y = x\\u2", 3L, 3L, 27L);
/*
 * Mark five points (coordinates in arrays XS and YS), using symbol
 * number 9.
 */
    n = 5;
    symbol = 9;
    pgpoint_(&n, xs, ys, &symbol);
/*
 * Compute the function at 60 points, and use PGLINE to draw it.
 */
    n = 60;
    for (i=0; i<n; i++)
        {
        xr[i] = 0.1*i;
        yr[i] = xr[i]*xr[i];
        }
    pgline_(&n, xr, yr);
/*
 * Finally, call PGEND to terminate things properly.
 */
    pgend_();
}
