C$Procedure      F_ZZFGEO ( Test private "fast" geometry routines )
 
      SUBROUTINE F_ZZFGEO ( OK )
      IMPLICIT NONE

C$ Abstract
C
C     Test private SPICELIB "fast" geometry routines.
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
C     This routine tests the private routine routines
C
C        ZZINRYPL
C        ZZRYXSPH
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
C-    TSPICE Version 1.0.0, 11-JAN-2016 (NJB)
C
C-&

 
C
C     Test Utility Functions
C
 

C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX
C      DOUBLE PRECISION      VNORM

C
C     Other functions
C
      DOUBLE PRECISION      T_RANDD

C
C     Local parameters
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-11 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-13 )


C      INTEGER               LNSIZE
C      PARAMETER           ( LNSIZE = 80 )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )

      INTEGER               TITLEN
      PARAMETER           ( TITLEN = 240 )

C
C     Local Variables
C       
C      CHARACTER*(LNSIZE)    LABEL
      CHARACTER*(TITLEN)    TITLE

      DOUBLE PRECISION      CONST
      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      PLANE  ( UBPL )
      DOUBLE PRECISION      MAXD
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      UDIR   ( 3 )
      DOUBLE PRECISION      UNORML ( 3 )
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XXPT   ( 3 )

      INTEGER               CASENO
C      INTEGER               I
      INTEGER               NCASE
      INTEGER               NXPTS
      INTEGER               SEED
      INTEGER               XNXPTS

      LOGICAL               FOUND
      LOGICAL               XFOUND
C
C     Save all variables to avoid stack problems on some
C     platforms.
C
      SAVE


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZFGEO' )


C**********************************************************************
C
C     ZZINRYPL tests
C
C**********************************************************************

 

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZINRYPL: basic test with plane parallel to X-Y '
     .//           'axes.'                                           )
 
      CALL VPACK ( 0.D0, 0.D0, 1.D0, NORMAL )

      CONST = 1.D0

      CALL VPACK (  2.D0, 4.D0,  2.D0, VERTEX )
      CALL VPACK ( -1.D0, 0.D0, -1.D0, DIR    )

      CALL VHAT ( DIR, UDIR )

C
C     Compute expected result.
C
      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ',  OK    )

      CALL INRYPL ( VERTEX, UDIR, PLANE, XNXPTS, XXPT ) 
      CALL CHCKXC ( .FALSE., ' ',  OK    )

      CALL CHCKSI ( 'ZNXPTS', XNXPTS, '=', 1, 0, OK )

C
C     Compute result using ZZINRYPL.
C
      MAXD = DPMAX()

      CALL ZZINRYPL ( VERTEX, UDIR, NORMAL, 
     .                CONST,  MAXD, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ',  OK    )
      
      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

      IF ( OK ) THEN
         CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, VTIGHT, OK )
      END IF

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZINRYPL: ray lies in plane.' )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, NORMAL )

      CONST = 0.D0

      CALL VPACK (  2.D0, 4.D0,  0.D0, VERTEX )

      CALL VPACK ( -1.D0, 0.D0,  0.D0, DIR    )

      CALL VHAT ( DIR, UDIR )


C
C     Compute result using ZZINRYPL.
C
      MAXD = DPMAX()

      CALL ZZINRYPL ( VERTEX, UDIR, NORMAL, 
     .                CONST,  MAXD, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ',  OK    )
      
C
C     This is a non-intersection case for ZZINRYPL.
C
      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZINRYPL: ray vertex only lies in plane.' )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, NORMAL )

      CONST = 0.D0

      CALL VPACK (  2.D0, 4.D0,  0.D0, VERTEX )

      CALL VPACK ( -1.D0, 0.D0,  -1.D0, DIR    )

      CALL VHAT ( DIR, UDIR )


C
C     Compute result using ZZINRYPL.
C
      MAXD = DPMAX()

      CALL ZZINRYPL ( VERTEX, UDIR, NORMAL, 
     .                CONST,  MAXD, NXPTS,  XPT )
      CALL CHCKXC ( .FALSE., ' ',  OK    )
      
C
C     This is an intersection case for ZZINRYPL.
C
      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

      IF ( OK ) THEN
         CALL CHCKAD ( 'XPT', XPT, '~~/', VERTEX, 3, TIGHT, OK )
      END IF


C
C --- Case --------------------------------------------------------
C

C
C     ZZINRYPL random tests
C

      SEED = -1

      NCASE = 10000

      DO CASENO = 1, NCASE

         SCALE      = 10.D0 ** T_RANDD ( -10.D0, 10.D0, SEED )

         NORMAL(1)  = T_RANDD ( -1.D0, 1.D0, SEED )
         NORMAL(2)  = T_RANDD ( -1.D0, 1.D0, SEED )
         NORMAL(3)  = T_RANDD ( -1.D0, 1.D0, SEED )

         CALL VHAT ( NORMAL, UNORML )
         CALL CHCKXC ( .FALSE., ' ',  OK    )
 
         CONST      = SCALE

         CALL NVC2PL ( UNORML, CONST, PLANE )
         CALL CHCKXC ( .FALSE., ' ',  OK    )

         DIR(1)  = T_RANDD ( -1.D0, 1.D0, SEED )
         DIR(2)  = T_RANDD ( -1.D0, 1.D0, SEED )
         DIR(3)  = T_RANDD ( -1.D0, 1.D0, SEED )

         CALL VHAT ( DIR, UDIR )
         CALL CHCKXC ( .FALSE., ' ',  OK    )
 
         VERTEX(1)  = T_RANDD ( -1.D0, 1.D0, SEED ) * SCALE
         VERTEX(2)  = T_RANDD ( -1.D0, 1.D0, SEED ) * SCALE
         VERTEX(3)  = T_RANDD ( -1.D0, 1.D0, SEED ) * SCALE


C
C --- Case --------------------------------------------------------
C
         TITLE = 'Scale = #; unit normal = (#,#,#); const = #; '
     .   //      'UDIR = (#,#,#), VERTEX = (#,#,#).'

         CALL REPMD ( TITLE, '#', SCALE,     7, TITLE )
         CALL REPMD ( TITLE, '#', UNORML(1), 7, TITLE )
         CALL REPMD ( TITLE, '#', UNORML(2), 7, TITLE )
         CALL REPMD ( TITLE, '#', UNORML(3), 7, TITLE )
         CALL REPMD ( TITLE, '#', CONST,     7, TITLE )
         CALL REPMD ( TITLE, '#', UDIR(1),   7, TITLE )
         CALL REPMD ( TITLE, '#', UDIR(2),   7, TITLE )
         CALL REPMD ( TITLE, '#', UDIR(3),   7, TITLE )
         CALL REPMD ( TITLE, '#', VERTEX(1), 7, TITLE )
         CALL REPMD ( TITLE, '#', VERTEX(2), 7, TITLE )
         CALL REPMD ( TITLE, '#', VERTEX(3), 7, TITLE )
         CALL CHCKXC ( .FALSE., ' ',  OK    )

         CALL TCASE ( TITLE  )


         CALL INRYPL ( VERTEX, UDIR, PLANE, XNXPTS, XXPT ) 
         CALL CHCKXC ( .FALSE., ' ',  OK    )

C
C        Compute result using ZZINRYPL.
C
         MAXD = 1.D10 * SCALE

         CALL ZZINRYPL ( VERTEX, UDIR, UNORML, 
     .                   CONST,  MAXD, NXPTS,  XPT )
         CALL CHCKXC ( .FALSE., ' ',  OK    )
      
         IF ( ( XNXPTS .EQ. 1 ) .OR. ( XNXPTS .EQ. 0 ) ) THEN

            CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

            IF ( OK .AND. ( NXPTS .EQ. 1 ) ) THEN
               CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TIGHT, OK )
            END IF         

         END IF

      END DO






C**********************************************************************
C
C     ZZRYXSPH tests
C
C**********************************************************************


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Unit sphere, vertex at center' )

      CALL CLEARD ( 3, VERTEX )

      DIR(1) = 1.D0
      DIR(2) = 0.D0
      DIR(3) = 0.D0
      
      R      = 1.D0

      CALL ZZRYXSPH ( VERTEX, DIR, R, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK    )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL VPACK ( 1.D0, 0.D0, 0.D0, XXPT )

      IF ( OK .AND. FOUND ) THEN

         CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TIGHT, OK )

      END IF

       
C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Unit sphere, vertex on Z axis at Z=2; DIR '
     .//           'points in -Z direction.'                   )

      CALL VPACK ( 0.D0, 0.D0, 2.D0, VERTEX )

      DIR(1) =  0.D0
      DIR(2) =  0.D0
      DIR(3) = -1.D0
      
      R      =  1.D0

      CALL ZZRYXSPH ( VERTEX, DIR, R, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK    )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, XXPT )

      IF ( OK .AND. FOUND ) THEN

         CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TIGHT, OK )

      END IF

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Unit sphere, vertex on line X=1,Y=2 at Z=2; DIR '
     .//           'points in -Z direction. Miss expected.'         )

      CALL VPACK ( 1.D0, 2.D0, 2.D0, VERTEX )

      DIR(1) =  0.D0
      DIR(2) =  0.D0
      DIR(3) = -1.D0
      
      R      =  1.D0

      CALL ZZRYXSPH ( VERTEX, DIR, R, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK    )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )



C
C --- Case --------------------------------------------------------
C

C
C     ZZRYXSPH random tests
C

      SEED = -1

      NCASE = 10000

      DO CASENO = 1, NCASE

         SCALE  = 10.D0 ** T_RANDD ( -10.D0, 10.D0, SEED )

         R      = T_RANDD ( 1.D0, 2.D0, SEED ) * SCALE / 2
  
         VERTEX(1)  = T_RANDD ( -1.D0, 1.D0, SEED ) * SCALE 
         VERTEX(2)  = T_RANDD ( -1.D0, 1.D0, SEED ) * SCALE 
         VERTEX(3)  = T_RANDD ( -1.D0, 1.D0, SEED ) * SCALE 

         DIR(1) = - VERTEX(1) +  T_RANDD ( -1.D0, 1.D0, SEED ) / 2
         DIR(2) = - VERTEX(2) +  T_RANDD ( -1.D0, 1.D0, SEED ) / 2
         DIR(3) = - VERTEX(3) +  T_RANDD ( -1.D0, 1.D0, SEED ) / 2

         CALL VHAT ( DIR, UDIR )
         CALL CHCKXC ( .FALSE., ' ',  OK    )

C
C --- Case --------------------------------------------------------
C
         TITLE = 'Scale = #; unit normal = (#,#,#); const = #; '
     .   //      'UDIR = (#,#,#), VERTEX = (#,#,#).'

         CALL REPMD ( TITLE, '#', SCALE,     7, TITLE )
         CALL REPMD ( TITLE, '#', UNORML(1), 7, TITLE )
         CALL REPMD ( TITLE, '#', UNORML(2), 7, TITLE )
         CALL REPMD ( TITLE, '#', UNORML(3), 7, TITLE )
         CALL REPMD ( TITLE, '#', CONST,     7, TITLE )
         CALL REPMD ( TITLE, '#', UDIR(1),   7, TITLE )
         CALL REPMD ( TITLE, '#', UDIR(2),   7, TITLE )
         CALL REPMD ( TITLE, '#', UDIR(3),   7, TITLE )
         CALL REPMD ( TITLE, '#', VERTEX(1), 7, TITLE )
         CALL REPMD ( TITLE, '#', VERTEX(2), 7, TITLE )
         CALL REPMD ( TITLE, '#', VERTEX(3), 7, TITLE )
         CALL CHCKXC ( .FALSE., ' ',  OK    )

         CALL TCASE ( TITLE  )


         CALL INRYPL ( VERTEX, UDIR, PLANE, XNXPTS, XXPT ) 
         CALL CHCKXC ( .FALSE., ' ',  OK    )

C
C        Compute result using ZZINRYPL.
C        
         CALL SURFPT ( VERTEX, UDIR, R, R, R, XXPT, XFOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK    )

         CALL ZZRYXSPH ( VERTEX, UDIR, R, XPT, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK    )
      
C         WRITE (*,*) 'FOUND, ||V||-R = ', FOUND, VNORM(VERTEX)-R

         CALL CHCKSL ( 'FOUND', FOUND, XFOUND, OK )

         IF ( OK .AND. FOUND ) THEN

            CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TIGHT, OK )

         END IF

      END DO





      CALL T_SUCCESS ( OK )
      RETURN
      END
