C$Procedure      F_SRFTRN ( Test surface name/ID mapping routines )
 
      SUBROUTINE F_SRFTRN ( OK )
      IMPLICIT NONE

C$ Abstract
C
C     Test the SPICELIB surface name/ID mapping routines.
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
C     This routine tests the surface name/ID mapping routines
C
C        SRFC2S
C        SRFCSS
C        SRFS2C
C        SRFSCC
C        ZZSRFC2N
C        ZZSRFINI
C        ZZSRFKER
C        ZZSRFN2C
C        ZZSRFTRK
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
C-    TSPICE Version 1.0.0, 04-DEC-2015 (NJB)
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
      CHARACTER*(*)         KVNNAM
      PARAMETER           ( KVNNAM = 'NAIF_SURFACE_NAME' )

      CHARACTER*(*)         KVBNAM
      PARAMETER           ( KVBNAM = 'NAIF_SURFACE_BODY' )

      CHARACTER*(*)         KVCNAM
      PARAMETER           ( KVCNAM = 'NAIF_SURFACE_CODE' )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NUMLEN
      PARAMETER           ( NUMLEN = 11 )
 
      INTEGER               NBDNAM
      PARAMETER           ( NBDNAM =  3 )

C
C     Local Variables
C     
      CHARACTER*(BDNMLN)    BODLST ( NBDNAM )
      CHARACTER*(BDNMLN)    BODNAM ( MXNSRF )
      CHARACTER*(BDNMLN)    BDNAM2 ( MXNSRF + 3 )
      CHARACTER*(BDNMLN)    BODSTR
      CHARACTER*(SFNMLN)    KERNAM ( MXNSRF )
      CHARACTER*(SFNMLN)    KRNAM2 ( MXNSRF + 3 )
      CHARACTER*(LNSIZE)    LABEL
      CHARACTER*(SFNMLN)    NORNAM ( MXNSRF )
      CHARACTER*(NUMLEN)    NUMSTR
      CHARACTER*(SFNMLN)    SRFSTR
      CHARACTER*(SFNMLN)    EXPSTR

      INTEGER               BODYID
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               KERBID ( MXNSRF )
      INTEGER               KRBID2 ( MXNSRF + 3 )
      INTEGER               KERSID ( MXNSRF )
      INTEGER               KRSID2 ( MXNSRF + 3)
      INTEGER               N
      INTEGER               NCASE
      INTEGER               NKVAR
      INTEGER               SIDHLS ( NROOM )
      INTEGER               SIDPOL ( LBSNGL : NROOM )
      INTEGER               SIDIDX ( NROOM )
      INTEGER               SNMHLS ( NROOM )
      INTEGER               SNMPOL ( LBSNGL : NROOM )
      INTEGER               SNMIDX ( NROOM )
      INTEGER               SURFID
      INTEGER               USRCTR ( CTRSIZ )
      
      LOGICAL               EXTKER
      LOGICAL               FOUND
      LOGICAL               ISNAME
      LOGICAL               UPDATE

C
C     Saved variables
C
      SAVE

C
C     Initial values
C
      DATA                 BODLST / 'Mars', 
     .                              'Phobos',
     .                              'Moon'   /

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_SRFTRN' )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Setup: create kernel variables' )

C
C     Create a set of variables with multiple bodies for a given
C     surface name.
C
      NKVAR = 1998


      DO I = 1, NKVAR, 3
         
         DO J = 1, 3

            K = I - 1 + J

            KERSID(K) = K

            BODNAM(K) = BODLST(J)
            
            CALL BODS2C ( BODNAM(K), KERBID(K), FOUND )
            CALL CHCKXC ( .FALSE., ' ',  OK )
            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


            WRITE ( NUMSTR, '(I4.4)' ) I

            KERNAM(K) = 'Surface # name'

            CALL REPMC ( KERNAM(K), '#', NUMSTR, KERNAM(K) )

         END DO

      END DO


C
C     >>> We'll avoid overwriting 
C
C            KERNAM
C            KERBID
C            KERSID
C            BODNAM
C
C         in the rest of this routine. We'll use the variables
C
C            KRNAM2
C            KRBID2
C            KRSID2
C            BDNAM2
C
C         for temporary kernel variable values.
C



C***********************************************************************
C
C     ZZSRFTRK tests     
C
C***********************************************************************

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZSRFTRK: check update status before any other '
     .//           'ZZSRFTRN entry point has been initialized.'     )

      CALL ZZCTRUIN ( USRCTR )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      
      CALL ZZSRFTRK ( USRCTR, UPDATE )
      CALL CHCKXC ( .FALSE., ' ',  OK )
 
      CALL CHCKSL ( 'UPDATE 0', UPDATE, .TRUE., OK )

C
C     Make a second check.
C
      CALL ZZSRFTRK ( USRCTR, UPDATE )
      CALL CHCKXC ( .FALSE., ' ',  OK )
 
      CALL CHCKSL ( 'UPDATE 1', UPDATE, .FALSE., OK )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZSRFTRK: check update status after a map update.' )

C
C     Install variables in the kernel pool.
C
      CALL PCPOOL ( KVNNAM, NKVAR, KERNAM )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVBNAM, NKVAR, KERBID )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVCNAM, NKVAR, KERSID )
      CALL CHCKXC ( .FALSE., ' ',  OK )


      CALL ZZSRFTRK ( USRCTR, UPDATE )
      CALL CHCKXC ( .FALSE., ' ',  OK )
 
      CALL CHCKSL ( 'UPDATE 0', UPDATE, .TRUE., OK )

C
C     Make a second check.
C
      CALL ZZSRFTRK ( USRCTR, UPDATE )
      CALL CHCKXC ( .FALSE., ' ',  OK )
 
      CALL CHCKSL ( 'UPDATE 1', UPDATE, .FALSE., OK )




C***********************************************************************
C
C     ZZSRFKER tests     
C
C***********************************************************************

C
C     Most of the functionality of ZZSRFKER will be exercised by 
C     tests of higher-level routines. We'll test the error handling
C     here.
C


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZSRFKER: check EXTKER when map is not present' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ',  OK )

      
      CALL ZZSRFKER ( KERNAM, NORNAM, KERSID, KERBID, 
     .                EXTKER, N,      SNMHLS, SNMPOL, 
     .                SNMIDX, SIDHLS, SIDPOL, SIDIDX )
 
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'EXTKER', EXTKER, .FALSE., OK )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZSRFKER: array size mismatch' )

C
C     Install variables in the kernel pool.
C
      CALL PCPOOL ( KVNNAM, 10, KERNAM )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVBNAM, NKVAR, KERBID )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVCNAM, NKVAR, KERSID )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      
      CALL ZZSRFKER ( KERNAM, NORNAM, KERSID, KERBID, 
     .                EXTKER, N,      SNMHLS, SNMPOL, 
     .                SNMIDX, SIDHLS, SIDPOL, SIDIDX )
 
      CALL CHCKXC ( .TRUE., 'SPICE(ARRAYSIZEMISMATCH)',  OK )

      CALL PCPOOL ( KVNNAM, NKVAR, KERNAM )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVBNAM, 10,    KERBID )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVCNAM, NKVAR, KERSID )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      
      CALL ZZSRFKER ( KERNAM, NORNAM, KERSID, KERBID, 
     .                EXTKER, N,      SNMHLS, SNMPOL, 
     .                SNMIDX, SIDHLS, SIDPOL, SIDIDX )
 
      CALL CHCKXC ( .TRUE., 'SPICE(ARRAYSIZEMISMATCH)',  OK )



      CALL PCPOOL ( KVNNAM, NKVAR, KERNAM )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVBNAM, NKVAR, KERBID )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVCNAM, 10, KERSID )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      
      CALL ZZSRFKER ( KERNAM, NORNAM, KERSID, KERBID, 
     .                EXTKER, N,      SNMHLS, SNMPOL, 
     .                SNMIDX, SIDHLS, SIDPOL, SIDIDX )
 
      CALL CHCKXC ( .TRUE., 'SPICE(ARRAYSIZEMISMATCH)',  OK )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZSRFKER: bad kernel variable data type' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ',  OK )
      
C
C     Install variables in the kernel pool.
C
      CALL PCPOOL ( KVBNAM, NKVAR, KERNAM )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PCPOOL ( KVCNAM, NKVAR, KERNAM )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVNNAM, NKVAR, KERSID )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      
      CALL ZZSRFKER ( KERNAM, NORNAM, KERSID, KERBID, 
     .                EXTKER, N,      SNMHLS, SNMPOL, 
     .                SNMIDX, SIDHLS, SIDPOL, SIDIDX )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLETYPE)',  OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZSRFKER: arrays too large' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ',  OK )      

      DO I = 1, NKVAR

         KRNAM2(I) = KERNAM(I)
         KRBID2(I) = KERBID(I)
         KRSID2(I) = KERSID(I)

      END DO

      KRNAM2( NKVAR+1 ) = 'EXTRA 1'
      KRNAM2( NKVAR+2 ) = 'EXTRA 2'
      KRNAM2( NKVAR+3 ) = 'EXTRA 3'

      KRSID2( NKVAR+1 ) =  1
      KRSID2( NKVAR+1 ) =  2
      KRSID2( NKVAR+1 ) =  3 

      KRBID2( NKVAR+1 ) = -1
      KRBID2( NKVAR+1 ) = -2
      KRBID2( NKVAR+1 ) = -3 

C
C     Install variables in the kernel pool.
C
      CALL PCPOOL ( KVNNAM, NKVAR+3, KRNAM2 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVBNAM, NKVAR+3, KRBID2 )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      
      CALL PIPOOL ( KVCNAM, NKVAR+3, KRSID2 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL ZZSRFKER ( KRNAM2, NORNAM, KRSID2, KRBID2, 
     .                EXTKER, N,      SNMHLS, SNMPOL, 
     .                SNMIDX, SIDHLS, SIDPOL, SIDIDX )
 
      CALL CHCKXC ( .TRUE., 'SPICE(TOOMANYSURFACES)',  OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'ZZSRFKER: blank surface name' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ',  OK )      
    
C
C     Install variables in the kernel pool.
C
      KRNAM2(1000) = ' '

      CALL PCPOOL ( KVNNAM, NKVAR, KRNAM2 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVBNAM, NKVAR, KRBID2 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVCNAM, NKVAR, KRSID2 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL ZZSRFKER ( KRNAM2, NORNAM, KRSID2, KRBID2, 
     .                EXTKER, N,      SNMHLS, SNMPOL, 
     .                SNMIDX, SIDHLS, SIDPOL, SIDIDX )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BLANKNAMEASSIGNED)',  OK )
      


C***********************************************************************
C
C     SRFS2C tests     
C
C***********************************************************************

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'SRFS2C: map names to codes' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ',  OK )      

C
C     Install variables in the kernel pool.
C
      CALL PCPOOL ( KVNNAM, NKVAR, KERNAM )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVBNAM, NKVAR, KERBID )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVCNAM, NKVAR, KERSID )
      CALL CHCKXC ( .FALSE., ' ',  OK )



      NCASE = NKVAR

      DO I = 1, NCASE

         CALL SRFS2C ( KERNAM(I), BODNAM(I), SURFID, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         LABEL = 'FOUND #'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSL ( LABEL, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            LABEL = 'Surf ID #'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKSI ( LABEL, SURFID, '=', KERSID(I), 0, OK )

         END IF

      END DO
      

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'SRFS2C: map integer surface and body '
     .//           'strings to codes'                    )


      NCASE = NKVAR

      DO I = 1, NCASE

         CALL INTSTR ( KERSID(I), SRFSTR )
         CALL INTSTR ( KERBID(I), BODSTR )

         CALL SRFS2C ( SRFSTR, BODSTR, SURFID, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         LABEL = 'FOUND #'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSL ( LABEL, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            LABEL = 'Surf ID #'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKSI ( LABEL, SURFID, '=', KERSID(I), 0, OK )

         END IF

      END DO


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'SRFS2C: surface name not valid; body name valid' )

      SRFSTR = 'ZZZ'

      CALL INTSTR ( KERBID(1), BODSTR )


      CALL SRFS2C ( SRFSTR, BODSTR, SURFID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSL ( LABEL, FOUND, .FALSE., OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'SRFS2C: surface name valid; body name not valid' )

      BODSTR = 'ZZZ'

      CALL SRFS2C ( KERNAM(1), BODSTR, SURFID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSL ( LABEL, FOUND, .FALSE., OK )
  

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'SRFS2C: clear pool, make sure translations '
     .//           'are not performed.'                         )

      CALL KCLEAR 
      CALL CHCKXC ( .FALSE., ' ',  OK )

      DO I = 1, NCASE

         CALL SRFS2C ( KERNAM(I), BODNAM(I), SURFID, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         LABEL = 'FOUND #'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSL ( LABEL, FOUND, .FALSE., OK )

      END DO


      

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'SRFS2C: map names to codes (change variables)' )


C
C     Create variables having reversed order of elements; also 
C     negate surface IDs.
C     
      DO I = 1, NKVAR

         J = NKVAR + 1 - I

         KRNAM2(J) =  KERNAM(I)(:SFNMLN-2) // 'ZZ'
         KRBID2(J) =  KERBID(I)
         BDNAM2(J) =  BODNAM(I)
         KRSID2(J) = -KERSID(I)

      END DO

C
C     Install variables in the kernel pool.
C
      CALL PCPOOL ( KVNNAM, NKVAR, KRNAM2 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVBNAM, NKVAR, KRBID2 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVCNAM, NKVAR, KRSID2 )
      CALL CHCKXC ( .FALSE., ' ',  OK )       



      NCASE = NKVAR

      DO I = 1, NCASE

         CALL SRFS2C ( KRNAM2(I), BDNAM2(I), SURFID, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         LABEL = 'FOUND #'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSL ( LABEL, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            LABEL = 'Surf ID #'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKSI ( LABEL, SURFID, '=', KRSID2(I), 0, OK )

         END IF

      END DO

       
      

C***********************************************************************
C
C     SRFSCC tests     
C
C***********************************************************************

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'SRFSCC: map names to codes' )


C
C     Install variables in the kernel pool.
C
      CALL PCPOOL ( KVNNAM, NKVAR, KERNAM )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVBNAM, NKVAR, KERBID )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVCNAM, NKVAR, KERSID )
      CALL CHCKXC ( .FALSE., ' ',  OK )



      NCASE = NKVAR

      DO I = 1, NCASE

         CALL SRFSCC ( KERNAM(I), KERBID(I), SURFID, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         LABEL = 'FOUND #'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSL ( LABEL, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            LABEL = 'Surf ID #'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKSI ( LABEL, SURFID, '=', KERSID(I), 0, OK )

         END IF

      END DO



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'SRFSCC: map integer surface '
     .//           'strings to codes'            )


      NCASE = NKVAR

      DO I = 1, NCASE

         CALL INTSTR ( KERSID(I), SRFSTR )

         CALL SRFSCC ( SRFSTR, KERBID(I), SURFID, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         LABEL = 'FOUND #'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSL ( LABEL, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            LABEL = 'Surf ID #'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKSI ( LABEL, SURFID, '=', KERSID(I), 0, OK )

         END IF

      END DO



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'SRFSCC: surface name not valid; body code valid' )

      SRFSTR = 'ZZZ'

      CALL SRFSCC ( SRFSTR, KERBID(1), SURFID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSL ( LABEL, FOUND, .FALSE., OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'SRFSCC: surface name valid; body code not valid' )


      CALL SRFSCC ( KERNAM(1), -1, SURFID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSL ( LABEL, FOUND, .FALSE., OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'SRFSCC: clear pool, make sure translations '
     .//           'are not performed.'                         )

      CALL KCLEAR 
      CALL CHCKXC ( .FALSE., ' ',  OK )

      DO I = 1, NCASE

         CALL SRFSCC ( KERNAM(I), KERBID(I), SURFID, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         LABEL = 'FOUND #'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSL ( LABEL, FOUND, .FALSE., OK )

      END DO

C
C     Install variables in the kernel pool.
C
      CALL PCPOOL ( KVNNAM, NKVAR, KERNAM )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVBNAM, NKVAR, KERBID )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVCNAM, NKVAR, KERSID )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      

C***********************************************************************
C
C     SRFC2S tests     
C
C***********************************************************************

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'SRFC2S: map codes to names' )


      NCASE = NKVAR

      DO I = 1, NCASE

         CALL SRFC2S ( KERSID(I), KERBID(I), SRFSTR, ISNAME )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         LABEL = 'ISNAME #'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSL ( LABEL, ISNAME, .TRUE., OK )

         IF ( FOUND ) THEN

            LABEL = 'Surf name #'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKSC ( LABEL, SRFSTR, '=', KERNAM(I), OK )

         END IF

      END DO 


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'SRFCSS: surface code valid; body code not valid' )

      BODYID = -3

      CALL SRFC2S ( KERSID(1), BODYID, SRFSTR, ISNAME )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSL ( LABEL, ISNAME, .FALSE., OK )

      CALL INTSTR ( KERSID(1), EXPSTR )

      LABEL = 'Surf name #'
      CALL REPMI ( LABEL, '#', I, LABEL )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( LABEL, SRFSTR, '=', EXPSTR, OK )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'SRFC2S: map names to codes (change variables)' )

C
C     Create variables having reversed order of elements; also 
C     negate surface IDs.
C     
      DO I = 1, NKVAR

         J = NKVAR + 1 - I

         KRNAM2(J) =  KERNAM(I)(:SFNMLN-2) // 'ZZ'
         KRBID2(J) =  KERBID(I)
         BDNAM2(J) =  BODNAM(I)
         KRSID2(J) = -KERSID(I)

      END DO

C
C     Install variables in the kernel pool.
C
      CALL PCPOOL ( KVNNAM, NKVAR, KRNAM2 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVBNAM, NKVAR, KRBID2 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVCNAM, NKVAR, KRSID2 )
      CALL CHCKXC ( .FALSE., ' ',  OK )       


      DO I = 1, NCASE

         CALL SRFC2S ( KRSID2(I), KRBID2(I), SRFSTR, ISNAME )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         LABEL = 'ISNAME #'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSL ( LABEL, ISNAME, .TRUE., OK )

         IF ( FOUND ) THEN

            LABEL = 'Surf name #'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKSC ( LABEL, SRFSTR, '=', KRNAM2(I), OK )

         END IF

      END DO 

      

C***********************************************************************
C
C     SRFCSS tests     
C
C***********************************************************************

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'SRFCSS: map codes to names' )


C
C     Install variables in the kernel pool.
C
      CALL PCPOOL ( KVNNAM, NKVAR, KERNAM )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVBNAM, NKVAR, KERBID )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVCNAM, NKVAR, KERSID )
      CALL CHCKXC ( .FALSE., ' ',  OK )


      NCASE = NKVAR

      DO I = 1, NCASE

         CALL SRFCSS ( KERSID(I), BODNAM(I), SRFSTR, ISNAME )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         LABEL = 'ISNAME #'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSL ( LABEL, ISNAME, .TRUE., OK )

         IF ( FOUND ) THEN

            LABEL = 'Surf name #'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKSC ( LABEL, SRFSTR, '=', KERNAM(I), OK )

         END IF

      END DO 


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'SRFCSS: map codes to names using body strings' )


      NCASE = NKVAR

      DO I = 1, NCASE

         CALL INTSTR ( KERBID(I), BODSTR )

         CALL SRFCSS ( KERSID(I), BODSTR, SRFSTR, ISNAME )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         LABEL = 'ISNAME #'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSL ( LABEL, ISNAME, .TRUE., OK )

         IF ( FOUND ) THEN

            LABEL = 'Surf name #'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKSC ( LABEL, SRFSTR, '=', KERNAM(I), OK )

         END IF

      END DO 


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'SRFCSS: surface code valid; body string not valid' )

      BODSTR = 'ZZZ'

      CALL SRFCSS ( KERSID(1), BODSTR, SRFSTR, ISNAME )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSL ( LABEL, ISNAME, .FALSE., OK )

      CALL INTSTR ( KERSID(1), EXPSTR )

      LABEL = 'Surf name #'
      CALL REPMI ( LABEL, '#', I, LABEL )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( LABEL, SRFSTR, '=', EXPSTR, OK )

 



C***********************************************************************
C
C     ZZSRFN2C, ZZSRFC2N tests     
C
C***********************************************************************


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check mapping of alias surface names to codes'  )
      
C
C     Create a mapping such that, for each surface ID and body,
C     there are three names.
C
      DO I = 1, NKVAR, 3

         DO J = 1, 3 

            K = I - 1 + J

            KRNAM2(K) = KERNAM(K)

            CALL REPMC  ( KRNAM2(K), 'name', 'name #', KRNAM2(K) )
            CALL CHCKXC ( .FALSE., ' ',  OK )
            CALL REPMI  ( KRNAM2(K), '#',     J,       KRNAM2(K) )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            KRBID2(K) = KERBID(I)
            KRSID2(K) = KERSID(I)
            BDNAM2(K) = BODNAM(I)

         END DO
         
      END DO

     
C
C     Install variables in the kernel pool.
C
      CALL PCPOOL ( KVNNAM, NKVAR, KRNAM2 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVBNAM, NKVAR, KRBID2 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVCNAM, NKVAR, KRSID2 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      DO I = 1, NKVAR

         CALL ZZSRFN2C ( KRNAM2(I), KRBID2(I), SURFID, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         LABEL = 'FOUND #'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSL ( LABEL, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            LABEL = 'Surf ID #'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKSI ( LABEL, SURFID, '=', KRSID2(I), 0, OK )

         END IF         

      END DO


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check mapping of codes to aliased surface names'  )

C
C     Use the mapping from the previous test case.
C
C     Make sure the highest-priority name is selected.
C

      DO I = 1, NKVAR, 3

         CALL ZZSRFC2N ( KRSID2(I), KRBID2(I), SRFSTR, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         LABEL = 'FOUND #'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSL ( LABEL, FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            LABEL = 'Surf string #'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ',  OK )

C
C           Note the index used in KRNAM2...
C
            CALL CHCKSC ( LABEL, SRFSTR, '=', KRNAM2(I+2), OK )

         END IF         

      END DO


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check masking of surface names associated with '
     .//           'multiple codes when mapping names to codes'     )


C
C     Create a mapping such that, for each surface name and body,
C     there are three surface IDs.
C
      DO I = 1, NKVAR, 3

         DO J = 1, 3 

            K = I - 1 + J

            KRNAM2(K) = KERNAM(I)
            KRBID2(K) = KERBID(I)
            KRSID2(K) = KERSID(K)
            BDNAM2(K) = BODNAM(I)

         END DO
         
      END DO

C
C     Install variables in the kernel pool.
C
      CALL PCPOOL ( KVNNAM, NKVAR, KRNAM2 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVBNAM, NKVAR, KRBID2 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL PIPOOL ( KVCNAM, NKVAR, KRSID2 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      DO I = 1, NKVAR, 3
C
C        The mapping in this direction need be performed for
C        only one value of J.
C
         DO J = 1, 1

            K = I - 1 + J

            CALL ZZSRFN2C ( KRNAM2(I), KRBID2(I), SURFID, FOUND )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            LABEL = 'FOUND #'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKSL ( LABEL, FOUND, .TRUE., OK )

            IF ( FOUND ) THEN
  
               LABEL = 'Surf ID #'
               CALL REPMI ( LABEL, '#', I, LABEL )
               CALL CHCKXC ( .FALSE., ' ',  OK )
C
C              Note index of KRSID2.
C
               CALL CHCKSI ( LABEL, SURFID, '=', KRSID2(I+2), 0, OK )

            END IF   

         END DO

      END DO

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check masking of surface names associated with '
     .//           'multiple codes when mapping codes to names'     )

C
C     Use the mapping defined in the previous test case.
C

      DO I = 1, NKVAR, 3
C
C        The mapping in this direction can be performed for
C        only J = 3.
C
         DO J = 1, 2

            K = I - 1 + J

            CALL ZZSRFC2N ( KRSID2(K), KRBID2(K), SRFSTR, FOUND )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            LABEL = 'FOUND #'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKSL ( LABEL, FOUND, .FALSE., OK )

         END DO


         DO J = 3, 3 

            K = I - 1 + J

            CALL ZZSRFC2N ( KRSID2(K), KRBID2(K), SRFSTR, FOUND )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            LABEL = 'FOUND #'
            CALL REPMI ( LABEL, '#', K, LABEL )
            CALL CHCKXC ( .FALSE., ' ',  OK )

            CALL CHCKSL ( LABEL, FOUND, .TRUE., OK )

            IF ( FOUND ) THEN
  
               LABEL = 'Surf name #'
               CALL REPMI ( LABEL, '#', K, LABEL )
               CALL CHCKXC ( .FALSE., ' ',  OK )

               CALL CHCKSC ( LABEL, SRFSTR, '=', KRNAM2(K), OK )

            END IF   

         END DO

      END DO


      CALL T_SUCCESS ( OK )
      RETURN
      END
