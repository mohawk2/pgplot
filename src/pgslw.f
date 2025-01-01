C*PGSLW -- set line width
C+
      SUBROUTINE PGSLW (LW)
      INTEGER  LW
C
C Set the line-width attribute. This attribute affects lines, graph
C markers, and text. Thick lines are generated by tracing each line
C with multiple strokes offset in the direction perpendicular to the
C line. The line width is specified by the number of strokes to be
C used, which must be in the range 1-201. The actual line width
C obtained depends on the device resolution.
C
C Argument:
C  LW     (input)  : the number of strokes to be used
C                    (in range 1-201).
C--
C  8-Aug-1985 - new routine, equivalent to GRSLW [TJP].
C-----------------------------------------------------------------------
      CALL GRSLW(LW)
      END
