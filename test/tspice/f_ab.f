C$Procedure      F_AB ( Test DSK AB list routines )
 
      SUBROUTINE F_AB ( OK )

C$ Abstract
C
C     Test the AB list routines used by the DSK subsystem.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     DSK
C
C$ Keywords
C
C     TEST 
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE 
      LOGICAL               OK

      INCLUDE 'dsk02.inc'
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting. The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the private AB list routines
C
C        ZZADDLNK
C        ZZINILNK
C        ZZTRVLNK
C        ZZUNTNGL
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     This routine currently does not exercise DSKX02.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C     W.L. Taber       (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 06-JAN-2016  (NJB)
C
C-&
 
 
C
C     Test Utility Functions
C
 

C
C     SPICELIB functions
C

C
C     Local parameters
C
      INTEGER               MXCELL
      PARAMETER           ( MXCELL = 300 )

      INTEGER               MAXA
      PARAMETER           ( MAXA = 100 )

      INTEGER               MAXB
      PARAMETER           ( MAXB = 200 )

      INTEGER               NULPTR
      PARAMETER           ( NULPTR = -1 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               NAVALS
      PARAMETER           ( NAVALS = 3 )

C
C     Local Variables
C 
      CHARACTER*(LNSIZE)    LABEL

      INTEGER               ALIST  ( MAXA )
      INTEGER               AVALS  ( NAVALS )
      INTEGER               BLIST  ( MAXB )
      INTEGER               BVAL
      INTEGER               CELLS  ( 2, MXCELL )
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               N
      INTEGER               NB
      INTEGER               NCELL
      INTEGER               XVAL
      INTEGER               XVALS  ( MXCELL )
           
 
C
C     Saved variables
C
      SAVE

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_AB' )


C***********************************************************************
C
C     ZZINILNK tests
C
C***********************************************************************

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Bad cell count' )
 
      CALL ZZINILNK ( MAXA, 0, NCELL, ALIST, CELLS )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Bad pointer count' )
 
      CALL ZZINILNK ( 0, MXCELL, NCELL, ALIST, CELLS )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Initialize an AB list' )

 
      CALL ZZINILNK ( MAXA, MXCELL, NCELL, ALIST, CELLS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The A list should have all entries initialized to "null."
C
      CALL FILLI ( NULPTR, MAXA, XVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAI ( 'ALIST', ALIST, '=', XVALS, MAXA, OK )

C
C     The cell array should have all value elements set to 0.
C
      DO I = 1, MXCELL

         LABEL = 'CELLS(1,@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, CELLS(1,I), '=', 0, 0, OK )

      END DO

C
C     The cell array should have all pointer elements set to NULPTR.
C
      DO I = 1, MXCELL

         LABEL = 'CELLS(2,@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, CELLS(2,I), '=', NULPTR, 0, OK )

      END DO

C
C     The pointer array should have all pointer elements set to NULPTR.
C
      DO I = 1, MAXA

         LABEL = 'ALIST(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, ALIST(I), '=', NULPTR, 0, OK )

      END DO

C
C     The used cell count should be zero on output.
C
      CALL CHCKSI ( 'NCELL', NCELL, '=', 0, 0, OK )


C***********************************************************************
C
C     ZZADDLNK tests
C
C***********************************************************************

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'AVAL out of range' )
 
      CALL ZZINILNK ( MAXA, MXCELL, NCELL, ALIST, CELLS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZADDLNK ( 0, 1, MAXA, MXCELL, ALIST, NCELL, CELLS )
      CALL CHCKXC ( .TRUE., 'SPICE(AVALOUTOFRANGE)', OK )


      CALL ZZADDLNK ( -1, 1, MAXA, MXCELL, ALIST, NCELL, CELLS )
      CALL CHCKXC ( .TRUE., 'SPICE(AVALOUTOFRANGE)', OK )


      CALL ZZADDLNK ( MAXA+1, 1, MAXA, MXCELL, ALIST, NCELL, CELLS )
      CALL CHCKXC ( .TRUE., 'SPICE(AVALOUTOFRANGE)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Too many cell entries.' )

 
      CALL ZZINILNK ( MAXA, MXCELL, NCELL, ALIST, CELLS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, MXCELL 

         CALL ZZADDLNK ( 1, I, MAXA, MXCELL, ALIST, NCELL, CELLS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL ZZADDLNK ( 1, I+1, MAXA, MXCELL, ALIST, NCELL, CELLS )
      CALL CHCKXC ( .TRUE., 'SPICE(CELLARRAYTOOSMALL)', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Build trivial list for one A-value.' )

 
      CALL ZZINILNK ( MAXA, MXCELL, NCELL, ALIST, CELLS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, MXCELL 

         CALL ZZADDLNK ( 1, I, MAXA, MXCELL, ALIST, NCELL, CELLS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Check NCELL.
C
      CALL CHCKSI ( 'NCELL', NCELL, '=', MXCELL, 0, OK )

C 
C     Check the pointer array. The first pointer should point to
C     the last cell; the other pointers should be null.
C
      CALL CHCKSI ( 'ALIST(1)', ALIST(1), '=', MXCELL, 0, OK )
      
      DO I = 2, MAXA

         LABEL = 'ALIST(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, ALIST(I), '=', -1, 0, OK )

      END DO

C
C     Check the cell entries.
C      
      DO I = MXCELL, 1, -1

         LABEL = 'CELLS(1,@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, CELLS(1,I), '=', I, 0, OK )


         LABEL = 'CELLS(2,@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( I .GT. 1 ) THEN

            XVAL = I-1
         ELSE
            XVAL =  -1
         END IF

         CALL CHCKSI ( LABEL, CELLS(2,I), '=', XVAL, 0, OK )

      END DO


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Build a singleton list for each A-value.' )

 
      CALL ZZINILNK ( MAXA, MXCELL, NCELL, ALIST, CELLS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = MAXA, 1, -1

         CALL ZZADDLNK ( I, -I, MAXA, MXCELL, ALIST, NCELL, CELLS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Check NCELL.
C
      CALL CHCKSI ( 'NCELL', NCELL, '=', MAXA, 0, OK )

C 
C     Check the pointer array. The Ith pointer should point to
C     the cell at index MAXA+1-I.
C      
      DO I = 1, MAXA

         LABEL = 'ALIST(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, ALIST(I), '=', MAXA+1-I, 0, OK )

      END DO

C
C     Check the cell entries.
C      
      DO I = 1, MAXA

         LABEL = 'CELLS(1,@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XVAL = -( MAXA+1-I )

         CALL CHCKSI ( LABEL, CELLS(1,I), '=', XVAL, 0, OK )


         LABEL = 'CELLS(2,@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XVAL =  -1

         CALL CHCKSI ( LABEL, CELLS(2,I), '=', XVAL, 0, OK )

      END DO



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Build a list for each of three A-values, where ' 
     .//           'the list additions are interleaved.'           )

 
      CALL ZZINILNK ( MAXA, MXCELL, NCELL, ALIST, CELLS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      AVALS(1) = 4
      AVALS(2) = 1
      AVALS(3) = 30

      N        = 20

      DO I = 1, N

         DO J = 1, NAVALS

            BVAL = ( J * 1000 ) + I

            CALL ZZADDLNK ( AVALS(J), BVAL,  MAXA, MXCELL, 
     .                      ALIST,    NCELL, CELLS        )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO


C
C     Check NCELL.
C
      CALL CHCKSI ( 'NCELL', NCELL, '=', NAVALS*N, 0, OK )
 
C
C     Recover the B values for each A value.
C
      DO I = 1, NAVALS

         J    = ALIST( AVALS(I) )

         DO K = 1, N

            BVAL = CELLS( 1,J )
        
            XVAL = ( I * 1000 ) + ( N + 1 - K )

            LABEL = 'BLIST(@) for AVAL @'
            CALL REPMI ( LABEL, '@', K, LABEL )
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSI ( LABEL, BVAL, '=', XVAL, 0, OK )


            J    = CELLS( 2,J ) 

         END DO

      END DO



C***********************************************************************
C
C     ZZTRVLNK tests
C
C***********************************************************************

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Collect the B-lists for AVALS from the previous '
     .//           'ZZADDLNK case.'                                  )


      DO I = 1, NAVALS

         CALL ZZTRVLNK ( AVALS(I), MAXA, ALIST, MXCELL,
     .                   CELLS,    MAXB, NB,    BLIST   )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check size of returned list.
C      
         CALL CHCKSI ( 'NB', NB, '=', N, 0, OK )

C
C        Check the list itself.
C
         DO K = 1, N

            XVAL = ( I * 1000 ) + ( N + 1 - K )

            LABEL = 'BLIST(@) for AVAL @'
            CALL REPMI ( LABEL, '@', K, LABEL )
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSI ( LABEL, BLIST(K), '=', XVAL, 0, OK )

         END DO

      END DO


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Pointer for AVAL is null.' )

      I = -1
      CALL ZZTRVLNK ( 1,     MAXA, I,  MXCELL,
     .                CELLS, MAXB, NB, BLIST   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check size of returned list.
C      
      CALL CHCKSI ( 'NB', NB, '=', 0, 0, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Pointer for AVAL is out of range.' )

      I = -2
      CALL ZZTRVLNK ( 1,     MAXA, I,  MXCELL,
     .                CELLS, MAXB, NB, BLIST   )
      CALL CHCKXC ( .TRUE., 'SPICE(POINTEROUTOFRANGE)', OK )

      I =  0
      CALL ZZTRVLNK ( 1,     MAXA, I,  MXCELL,
     .                CELLS, MAXB, NB, BLIST   )
      CALL CHCKXC ( .TRUE., 'SPICE(POINTEROUTOFRANGE)', OK )
 
      I = MXCELL + 1
      CALL ZZTRVLNK ( 1,     MAXA, I,  MXCELL,
     .                CELLS, MAXB, NB, BLIST   )
      CALL CHCKXC ( .TRUE., 'SPICE(POINTEROUTOFRANGE)', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'AVAL out of range.'  )

      CALL ZZTRVLNK ( 0,     MAXA, ALIST, MXCELL,
     .                CELLS, MAXB, NB,    BLIST   )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'MAXB out of range.'  )

      CALL ZZTRVLNK ( 1,     MAXA, ALIST, MXCELL,
     .                CELLS, 0,    NB,    BLIST   )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)', OK )


C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'BLIST too large'  )

      CALL ZZTRVLNK ( AVALS(1), MAXA, ALIST, MXCELL,
     .                CELLS,    N-1,  NB,    BLIST   )
      CALL CHCKXC ( .TRUE., 'SPICE(BARRAYTOOSMALL)', OK )



C***********************************************************************
C
C     ZZUNTNGL tests
C
C***********************************************************************

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Untangle the interleaved list structure from the '
     .//           'last ZZADDLNK case.'                              )


      CALL ZZUNTNGL ( MAXA,  MXCELL, CELLS, MAXB, 
     .                ALIST, NB,     BLIST        )
      CALL CHCKXC ( .FALSE., ' ', OK )

     
C
C     Check size of returned array.
C      
      CALL CHCKSI ( 'NB', NB, '=', NAVALS * (N+1), 0, OK )

      
C
C     Check the array itself.
C
      DO I = 1, NAVALS
C
C        Let J be the start index of the Ith list. A count
C        should be present at that index.
C
         J = ALIST( AVALS(I) )

         LABEL = 'Count for AVAL @'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, BLIST(J), '=', N, 0, OK )
           
C
C        Check the B-values for the Ith list.
C
         DO K = 1, N

            XVAL = ( I * 1000 ) + ( N + 1 - K )

            LABEL = 'BLIST(@) for AVAL @'
            CALL REPMI ( LABEL, '@', K, LABEL )
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSI ( LABEL, BLIST(J+K), '=', XVAL, 0, OK )

         END DO

      END DO


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Output array size too small.'  )


      CALL ZZUNTNGL ( MAXA,  MXCELL, CELLS, 1, 
     .                ALIST, NB,     BLIST        )
      CALL CHCKXC ( .TRUE., 'SPICE(BARRAYTOOSMALL)', OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
