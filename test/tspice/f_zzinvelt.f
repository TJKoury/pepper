C$Procedure F_ZZINVELT ( ZZINVELT tests )
 
      SUBROUTINE F_ZZINVELT ( OK )
 
C$ Abstract
C
C     Exercise the private SPICELIB routine ZZINVELT.
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
 
      IMPLICIT NONE

      INCLUDE 'dskdsc.inc'
      INCLUDE 'dsktol.inc'

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
C     This routine tests the private SPICELIB routine ZZINVELT.
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
C-    TSPICE Version 1.0.0, 03-JUN-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C 
      DOUBLE PRECISION      PI

C
C     Local Parameters
C

C
C     Local Variables 
C
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      L
      DOUBLE PRECISION      MARGIN
      DOUBLE PRECISION      P      ( 3 )

      INTEGER               CORSYS

      INTEGER               EXCLUD            
      LOGICAL               INSIDE
      LOGICAL               XIN
           
C
C     Saved values
C
      SAVE

C
C     Initial values
C

C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZINVELT' )


C***********************************************************************
C
C     Normal cases
C
C***********************************************************************
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RECSYS: interior case. Exclude none. Zero MARGIN.' )
 

      CORSYS = RECSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      EXCLUD = 0
      MARGIN = 0.D0


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0

      
      CALL VPACK ( 2.D0, -2.D0, 7.D0, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, 
     .                BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )
      

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RECSYS: interior case. Exclude none. '
     .//           'MARGIN > 0.' )
 

      CORSYS = RECSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      EXCLUD = 0
      MARGIN = 1.D-2


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0

      L           = BOUNDS(2,3) - BOUNDS(1,3)


      CALL VPACK ( 2.D0, -2.D0, 8.D0 + 0.5D0*MARGIN*L, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, 
     .                BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RECSYS: exterior case. Exclude none. '
     .//           'MARGIN > 0.' )
 

      CORSYS = RECSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      EXCLUD = 0
      MARGIN = 1.D-2


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0

      L           = BOUNDS(2,3) - BOUNDS(1,3)

      
      CALL VPACK ( 2.D0, -2.D0, 8.D0 + 1.5D0*MARGIN*L, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, 
     .                BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RECSYS: interior case. Exclude Z. '
     .//           'MARGIN > 0.' )
 

      CORSYS = RECSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      EXCLUD = 3
      MARGIN = 1.D-2


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0

      L           = BOUNDS(2,3) - BOUNDS(1,3)

      
      CALL VPACK ( 2.D0, -2.D0, 8.D0 + 1.5D0*MARGIN*L, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, 
     .                BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )
      


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'LATSYS: interior case. Exclude none. Zero MARGIN.' )
 

      CORSYS = LATSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      EXCLUD = 0
      MARGIN = 0.D0


      BOUNDS(1,1) =  -PI()/3
      BOUNDS(2,1) =   PI()/3

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/4

      BOUNDS(1,3) =   6.D0
      BOUNDS(2,3) =   8.D0

      
      CALL VPACK ( 7.D0, 0.D0, 0.D0, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, 
     .                BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )

 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'LATSYS: interior case. Exclude none. '
     .//           'MARGIN > 0.'                          )
 

      CORSYS = LATSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      EXCLUD = 0
      MARGIN = 1.D-2


      BOUNDS(1,1) =  -PI()/3
      BOUNDS(2,1) =   PI()/3

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/4

      BOUNDS(1,3) =   6.D0
      BOUNDS(2,3) =   8.D0

      
      CALL VPACK ( 8.D0 + 0.5D0*MARGIN*8.D0, 0.D0, 0.D0, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, 
     .                BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'LATSYS: exterior case. Exclude none. '
     .//           'MARGIN > 0.'                          )
 

      CORSYS = LATSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      EXCLUD = 0
      MARGIN = 1.D-2


      BOUNDS(1,1) =  -PI()/3
      BOUNDS(2,1) =   PI()/3

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/4

      BOUNDS(1,3) =   6.D0
      BOUNDS(2,3) =   8.D0

      
      CALL VPACK ( 8.D0 + 1.5D0*MARGIN*8.D0, 0.D0, 0.D0, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, 
     .                BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'LATSYS: interior case. Exclude radius. '
     .//           'MARGIN > 0.'                          )
 

      CORSYS = LATSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      EXCLUD = 3
      MARGIN = 1.D-2


      BOUNDS(1,1) =  -PI()/3
      BOUNDS(2,1) =   PI()/3

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/4

      BOUNDS(1,3) =   6.D0
      BOUNDS(2,3) =   8.D0

      
      CALL VPACK ( 8.D0 + 1.5D0*MARGIN*8.D0, 0.D0, 0.D0, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, 
     .                BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'PDTSYS: interior case. Exclude none. Zero MARGIN.' )
 

      CORSYS = PDTSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      CORPAR(1) = 8.D0
      CORPAR(2) = 0.5D0


      EXCLUD = 0
      MARGIN = 0.D0


      BOUNDS(1,1) =  -PI()/3
      BOUNDS(2,1) =   PI()/3

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/4

      BOUNDS(1,3) =  -1.D0
      BOUNDS(2,3) =   1.D0

      
      CALL GEOREC ( 0.D0, 0.D0, 0.5D0, CORPAR(1), CORPAR(2), P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, 
     .                BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )

        

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'PDTSYS: interior case. Exclude none. '
     .//           'MARGIN > 0.'                          )
 
      CORPAR(1) = 8.D0
      CORPAR(2) = 0.5D0


      EXCLUD = 0
      MARGIN = 1.D-2


      BOUNDS(1,1) =  -PI()/3
      BOUNDS(2,1) =   PI()/3

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/4

      BOUNDS(1,3) =  -1.D0
      BOUNDS(2,3) =   1.D0

      L           =   BOUNDS(2,3) - BOUNDS(1,3)

      
      CALL GEOREC ( 0.D0,      0.D0,      1.D0 + 0.5D0*L*MARGIN,
     .              CORPAR(1), CORPAR(2), P                       )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      CALL ZZINVELT ( P,      CORSYS, CORPAR, 
     .                BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )

      

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'PDTSYS: exterior case. Exclude none. '
     .//           'MARGIN > 0.'                          )
 

      CORSYS = PDTSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      CORPAR(1) = 8.D0
      CORPAR(2) = 0.5D0


      EXCLUD = 0
      MARGIN = 1.D-2


      BOUNDS(1,1) =  -PI()/3
      BOUNDS(2,1) =   PI()/3

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/4

      BOUNDS(1,3) =  -1.D0
      BOUNDS(2,3) =   1.D0

      L           =   BOUNDS(2,3) - BOUNDS(1,3)

      
      CALL GEOREC ( 0.D0,      0.D0,      1.D0 + 1.5D0*L*MARGIN,
     .              CORPAR(1), CORPAR(2), P                       )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      CALL ZZINVELT ( P,      CORSYS, CORPAR, 
     .                BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      XIN = .FALSE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'PDTSYS: interior case. Exclude altitude. '
     .//           'MARGIN > 0.'                              )
 

      CORSYS = PDTSYS

      CALL CLEARD ( NSYPAR, CORPAR )


      CORPAR(1) = 8.D0
      CORPAR(2) = 0.5D0

      EXCLUD = 3
      MARGIN = 1.D-2


      BOUNDS(1,1) =  -PI()/3
      BOUNDS(2,1) =   PI()/3

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/4

      BOUNDS(1,3) =   6.D0
      BOUNDS(2,3) =   8.D0

      
      CALL VPACK ( 8.D0 + 1.5D0*MARGIN*8.D0, 0.D0, 0.D0, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, 
     .                BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )

 
 

C***********************************************************************
C
C     Error cases
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error: Negative margin.'  )

      CORSYS = PDTSYS

      CALL CLEARD ( NSYPAR, CORPAR )


      CORPAR(1) = 8.D0
      CORPAR(2) = 0.5D0

      EXCLUD = 3
      MARGIN = -1.D-2


      BOUNDS(1,1) =  -PI()/3
      BOUNDS(2,1) =   PI()/3

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/4

      BOUNDS(1,3) =   6.D0
      BOUNDS(2,3) =   8.D0

      
      CALL VPACK ( 8.D0 + 1.5D0*MARGIN*8.D0, 0.D0, 0.D0, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, 
     .                BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )  
      

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error: Invalid coordinate system code.'  )

      MARGIN = 0.D0

      CALL ZZINVELT ( P,      -1,     CORPAR, 
     .                BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )  

      CALL ZZINVELT ( P,       5,     CORPAR, 
     .                BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )  


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error: Invalid exclude index.'  )

      MARGIN = 0.D0

      EXCLUD = -1

      CALL ZZINVELT ( P,      PDTSYS, CORPAR, 
     .                BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )  


      EXCLUD = 4

      CALL ZZINVELT ( P,      PDTSYS, CORPAR, 
     .                BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )  



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error: invalid rectangular coordinate bounds.' )



      CORSYS = RECSYS

      CALL CLEARD ( NSYPAR, CORPAR )

      EXCLUD = 0
      MARGIN = 0.D0


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  -3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0

      
      CALL VPACK ( 2.D0, -2.D0, 7.D0, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, 
     .                BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSOUTOFORDER)', OK )  


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error: invalid planetodetic coordinate '
     .//           'parameters.'                            )

      CORSYS = PDTSYS

      CALL CLEARD ( NSYPAR, CORPAR )


      CORPAR(1) = -8.D0
      CORPAR(2) = 0.5D0

      EXCLUD = 3
      MARGIN = -1.D-2


      BOUNDS(1,1) =  -PI()/3
      BOUNDS(2,1) =   PI()/3

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/4

      BOUNDS(1,3) =   6.D0
      BOUNDS(2,3) =   8.D0

      
      CALL VPACK ( 8.D0 + 1.5D0*MARGIN*8.D0, 0.D0, 0.D0, P )

      CALL ZZINVELT ( P,      CORSYS, CORPAR, 
     .                BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )  
 
      CORPAR(1) = 8.D0
      CORPAR(2) = 1.1D0

      CALL ZZINVELT ( P,      CORSYS, CORPAR, 
     .                BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )  

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


