C*PGASK -- control new page prompting
C+
      SUBROUTINE PGASK (FLAG)
      LOGICAL FLAG
C
C Change the "prompt state" of PGPLOT. If the prompt state is
C ON, PGPAGE will type "Type <RETURN> for next page:" and will wait
C for the user to type <CR> before starting a new page.  The initial
C prompt state (after a call to PGBEG) is ON for interactive devices.
C Prompt state is always OFF for non-interactive devices.
C
C Arguments:
C  FLAG   (input)  : if .TRUE., and if the device is an interactive
C                    device, the prompt state will be set to ON. If
C                    .FALSE., the prompt state will be set to OFF.
C--
C-----------------------------------------------------------------------
      INCLUDE     'pgplot.inc'
      CHARACTER*1 TYPE
C
      IF (PGOPEN.EQ.0) RETURN
C
      IF (FLAG) THEN
          CALL GRQTYP(TYPE,PROMPT)
      ELSE
          PROMPT = .FALSE.
      END IF
      END
