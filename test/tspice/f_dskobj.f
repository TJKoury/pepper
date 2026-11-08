C$Procedure      F_DSKOBJ ( Test DSK body/surface coverage routines )
 
      SUBROUTINE F_DSKOBJ ( OK )
      IMPLICIT NONE

C$ Abstract
C
C     Test the SPICELIB DSK body/surface coverage routines.
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
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations

      INCLUDE 'srftrn.inc'
      INCLUDE 'zzctr.inc'
      INCLUDE 'zzsrftrn.inc'

      LOGICAL               OK
 
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
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the DSK body/surface coverage routines
C
C        DSKOBJ
C        DSKSRF
C        
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 16-MAY-2016 (NJB)
C
C-&

 
C
C     Test Utility Functions
C
 

C
C     SPICELIB functions
C
      INTEGER               CARDI
      INTEGER               ISRCHI

      LOGICAL               EXISTS

C
C     Local parameters
C
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0   = 'dskobj_test0.bds' )

      CHARACTER*(*)         DSK1
      PARAMETER           ( DSK1   = 'dskobj_test1.bds' )

      CHARACTER*(*)         SPK0
      PARAMETER           ( SPK0   = 'dskobj_test0.bsp' )

      CHARACTER*(*)         XFR0
      PARAMETER           ( XFR0   = 'dskobj_test0.xfr' )

      CHARACTER*(*)         EK0
      PARAMETER           ( EK0    = 'dskobj_test0.bes' )


      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 ) 
  
      INTEGER               NBOD
      PARAMETER           ( NBOD   = 10 )

      INTEGER               NDSK0
      PARAMETER           ( NDSK0  =  10 )

      INTEGER               NDSK1
      PARAMETER           ( NDSK1  = 1000 )

      INTEGER               MAXSIZ
      PARAMETER           ( MAXSIZ = 5000 )

C
C     Local Variables
C     
      CHARACTER*(FRNMLN)    FRNAME

      INTEGER               BODSET ( LBCELL : MAXSIZ )
      INTEGER               BODLST ( NBOD )
      INTEGER               BODYID
      INTEGER               FRAMID
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               N
      INTEGER               SRFSET ( LBCELL : MAXSIZ )
      INTEGER               UNIT
      INTEGER               XBDSET ( LBCELL : MAXSIZ )
      INTEGER               XN
      INTEGER               XSFSET ( LBCELL : MAXSIZ )

      LOGICAL               FOUND

C
C     Saved variables
C
      SAVE

C
C     Initial values
C
 

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_DSKOBJ' )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Setup: create DSK files' )

C
C     Start fresh. Since we're going to create a DSK and
C     then append to it, we don't want to create an ever-growing
C     monster file.
C 
      IF ( EXISTS(DSK0) ) THEN

         CALL DELFIL( DSK0 )
         CALL CHCKXC( .FALSE., ' ', OK )
      
      END IF

C
C     Create a body list.
C
      DO I = 1, NBOD-1
         BODLST(I) = 100*I + 99
      END DO
      
      BODLST(NBOD) = 10

C
C     Create a DSK file containing NDSK0 segments.
C
      DO I = 1, NDSK0
C
C        Generate a body ID for the Ith segment. Pick
C        a body from the body list. Recall we need to
C        restrict the body to the set for which there
C        are built-in body-fixed frame definitions.
C
         J      = 1 + MOD( I-1, NBOD )
         BODYID = BODLST(J)

         CALL CIDFRM ( BODYID, FRAMID, FRNAME, FOUND )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         CALL T_SMLDSK( BODYID, I, FRNAME, DSK0 )
         CALL CHCKXC( .FALSE., ' ', OK )

      END DO

C
C     Create a DSK file containing NDSK1 segments.
C
      IF ( EXISTS(DSK1) ) THEN

         CALL DELFIL( DSK1 )
         CALL CHCKXC( .FALSE., ' ', OK )
      
      END IF

      DO I = 1, NDSK1
C
C        Generate a body ID for the Ith segment. Pick
C        a body from the body list. Recall we need to
C        restrict the body to the set for which there
C        are built-in body-fixed frame definitions.
C
         J      = 1 + MOD( I-1, NBOD )
         BODYID = BODLST(J)

         CALL CIDFRM ( BODYID, FRAMID, FRNAME, FOUND )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         CALL T_SMLDSK( BODYID, -I, FRNAME, DSK1 )
         CALL CHCKXC( .FALSE., ' ', OK )

      END DO


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check the body set for DSK0.' )

C
C     Create the expected body set.
C
      CALL SSIZEI ( MAXSIZ, XBDSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      DO I = 1, NBOD
         CALL INSRTI ( BODLST(I), XBDSET )
         CALL CHCKXC( .FALSE., ' ', OK )
      END DO

C
C     Get the body set for DSK0.
C
      CALL SSIZEI ( MAXSIZ, BODSET )
      CALL CHCKXC( .FALSE., ' ', OK )
     
      CALL DSKOBJ ( DSK0, BODSET )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Check set cardinality.
C
      N  = CARDI( BODSET )
      XN = NBOD

      CALL CHCKSI ( 'CARDI(BODSET)', N, '=', XN, 0, OK )

      IF ( OK ) THEN
C
C        Check the body set.
C
         CALL CHCKAI ( 'BODSET', BODSET(1), '=', 
     .                           XBDSET(1), XN,  OK )
      END IF


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check the body set for DSK1.' )

C
C     Create the expected body set.
C
      CALL SSIZEI ( MAXSIZ, XBDSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      DO I = 1, NBOD
         CALL INSRTI ( BODLST(I), XBDSET )
         CALL CHCKXC( .FALSE., ' ', OK )
      END DO

C
C     Get the body set for DSK1.
C
      CALL SSIZEI ( MAXSIZ, BODSET )
      CALL CHCKXC( .FALSE., ' ', OK )
     
      CALL DSKOBJ ( DSK1, BODSET )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Check set cardinality.
C
      N  = CARDI( BODSET )
      XN = NBOD

      CALL CHCKSI ( 'CARDI(BODSET)', N, '=', XN, 0, OK )

      IF ( OK ) THEN
C
C        Check the body set.
C
         CALL CHCKAI ( 'BODSET', BODSET(1), '=', 
     .                           XBDSET(1), XN,  OK )
      END IF



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check the surface set for each body in DSK0.' )

      CALL SSIZEI ( MAXSIZ, SRFSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL SSIZEI ( MAXSIZ, XSFSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL SCARDI ( 0, BODSET )

      CALL DSKOBJ ( DSK0, BODSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      DO I = 1, CARDI( BODSET )

         CALL SCARDI( 0, SRFSET )
         CALL CHCKXC( .FALSE., ' ', OK )
C
C        Create the expected surface set.
C
         CALL SCARDI( 0, XSFSET )
         CALL CHCKXC( .FALSE., ' ', OK )

         BODYID = BODSET(I)

         J      = ISRCHI ( BODYID, NBOD, BODLST )
         K      = J

         DO WHILE ( K .LE. NDSK0 )
            
            CALL INSRTI ( K, XSFSET )
            CALL CHCKXC( .FALSE., ' ', OK )

            K = K + NBOD

         END DO

         XN = CARDI( XSFSET )


         CALL DSKSRF ( DSK0, BODSET(I), SRFSET )
         CALL CHCKXC( .FALSE., ' ', OK )

C
C        Check the cardinality of the surface set.
C
         N = CARDI( SRFSET )

         CALL CHCKSI ( 'CARDI(SRFSET)', N, '=', XN, 0, OK )

C
C        Check the surface set itself.
C
         IF ( OK ) THEN

            CALL CHCKAI ( 'SRFSET', SRFSET(1), '=', 
     .                              XSFSET(1), XN,  OK )
         END IF

      END DO

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check the surface set for each body in DSK1.' )

      CALL SSIZEI ( MAXSIZ, SRFSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL SSIZEI ( MAXSIZ, XSFSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL SCARDI ( 0, BODSET )

      CALL DSKOBJ ( DSK1, BODSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      DO I = 1, CARDI( BODSET )

         CALL SCARDI( 0, SRFSET )
         CALL CHCKXC( .FALSE., ' ', OK )
C
C        Create the expected surface set.
C
         CALL SCARDI( 0, XSFSET )
         CALL CHCKXC( .FALSE., ' ', OK )

         BODYID = BODSET(I)

         J      = ISRCHI ( BODYID, NBOD, BODLST )
         K      = J

         DO WHILE ( K .LE. NDSK1 )
            
            CALL INSRTI ( -K, XSFSET )
            CALL CHCKXC( .FALSE., ' ', OK )

            K = K + NBOD

         END DO

         XN = CARDI( XSFSET )


         CALL DSKSRF ( DSK1, BODSET(I), SRFSET )
         CALL CHCKXC( .FALSE., ' ', OK )

C
C        Check the cardinality of the surface set.
C
         N = CARDI( SRFSET )

         CALL CHCKSI( 'CARDI(SRFSET)', N, '=', XN, 0, OK )

C
C        Check the surface set itself.
C
         IF ( OK ) THEN

            CALL CHCKAI ( 'SRFSET', SRFSET(1), '=', 
     .                              XSFSET(1), XN,  OK )
         END IF

      END DO



C***********************************************************************
C
C     
C     DSKOBJ error cases
C     
C
C***********************************************************************


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKOBJ: output cell is too small.' )

      CALL SSIZEI( 3, BODSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DSKOBJ ( DSK0, BODSET )
      CALL CHCKXC ( .TRUE., 'SPICE(CELLTOOSMALL)', OK )

C
C     This call will leave the DSK closed.
C

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKOBJ: DSK doesn''t exist.' )

      CALL SCARDI ( 0, BODSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DSKOBJ ( 'XXX', BODSET )
      CALL CHCKXC( .TRUE., 'SPICE(FILENOTFOUND)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKOBJ: file is a non-DSK DAS.' )

      CALL TSTEK ( EK0, 1, 10, .FALSE., HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL SCARDI ( 0, BODSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DSKOBJ ( EK0, BODSET )
      CALL CHCKXC( .TRUE., 'SPICE(INVALIDFILETYPE)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKOBJ: file is a DAF.' )

      CALL TSTSPK ( SPK0, .FALSE., .TRUE. )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DSKOBJ ( SPK0, BODSET )
      CALL CHCKXC( .TRUE., 'SPICE(INVALIDARCHTYPE)', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKOBJ: file is an XFR file.' )

      IF ( EXISTS(XFR0) ) THEN

         CALL DELFIL( XFR0 )
         CALL CHCKXC( .FALSE., ' ', OK )

      END IF

      CALL TXTOPN( XFR0, UNIT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DASBT ( DSK0, UNIT )
      CALL CHCKXC( .FALSE., ' ', OK )
      
      CLOSE ( UNIT )

      CALL DSKOBJ ( XFR0, BODSET )
      CALL CHCKXC( .TRUE., 'SPICE(INVALIDFORMAT)', OK )



C***********************************************************************
C
C     
C     DSKSRF error cases
C     
C
C***********************************************************************


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKSRF: DSK doesn''t exist.' )

      CALL SCARDI ( 0, SRFSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DSKSRF ( 'XXX', 1, SRFSET )
      CALL CHCKXC( .TRUE., 'SPICE(FILENOTFOUND)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKSRF: file is a non-DSK DAS.' )


      CALL DSKSRF ( EK0, 1, SRFSET )
      CALL CHCKXC( .TRUE., 'SPICE(INVALIDFILETYPE)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKSRF: file is a DAF.' )

      CALL DSKSRF ( SPK0, 1, SRFSET )
      CALL CHCKXC( .TRUE., 'SPICE(INVALIDARCHTYPE)', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKSRF: file is an XFR file.' )

      CALL DSKSRF ( XFR0, 1, BODSET )
      CALL CHCKXC( .TRUE., 'SPICE(INVALIDFORMAT)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKSRF: output cell is too small.' )

      CALL SSIZEI( 3, BODSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DSKSRF ( DSK1, 199, BODSET )
      CALL CHCKXC ( .TRUE., 'SPICE(CELLTOOSMALL)', OK )

C
C     This call will leave the DSK closed.
C


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Clean up: delete kernels.' )


      CALL DELFIL ( SPK0 ) 
      CALL CHCKXC( .FALSE., ' ', OK )
     
      CALL DELFIL ( DSK0 ) 
      CALL CHCKXC( .FALSE., ' ', OK )
      
      CALL DELFIL ( DSK1 ) 
      CALL CHCKXC( .FALSE., ' ', OK )
      
      CALL DELFIL ( XFR0 ) 
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DELFIL ( EK0 ) 
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
