C$Procedure F_ZZDASGRC ( DAS record fetch tests )
 
      SUBROUTINE F_ZZDASGRC ( OK )
 
C$ Abstract
C
C     Exercise the private SPICELIB DAS record fetch routines.
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

      INCLUDE 'das.inc'

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
C     This routine tests the private SPICELIB DAS routines
C
C        ZZDASGRD
C        ZZDASGRI
C        ZZDASRFR
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
C-    TSPICE Version 1.0.0, 31-MAY-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C 
      LOGICAL               EXISTS
      LOGICAL               FAILED

C
C     Local Parameters
C
      CHARACTER*(*)         DSK
      PARAMETER           ( DSK    = 'zzdasgrc_test.bds' )

      CHARACTER*(*)         NNDSK
      PARAMETER           ( NNDSK  = 'nn_zzdasgrc_test.bds' )


      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'zzdasgrc_test.tpc' )
 

C
C     These parameters must be kept consistent with zzddhman.inc.
C     That file is not included because of a parameter name conflict
C     with das.inc.
C
      INTEGER               BIGI3E
      PARAMETER           ( BIGI3E = 1 )

      INTEGER               LTLI3E
      PARAMETER           ( LTLI3E = 2 )

C
C     Other parameters 
C
      INTEGER               LBLSIZ
      PARAMETER           ( LBLSIZ = 40 )

      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8 )

      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )
      
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               INTIX
      PARAMETER           ( INTIX  = 3 )
      
      INTEGER               DPIX
      PARAMETER           ( DPIX   = 2 )

C
C     Local Variables
C
      CHARACTER*(FRNMLN)    FRNAME
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(IFNLEN)    IFNAME
      CHARACTER*(LBLSIZ)    LABEL
      CHARACTER*(IDWLEN)    XIDW
      CHARACTER*(IFNLEN)    XIFNAM

      DOUBLE PRECISION      DREC   ( NWD )
      DOUBLE PRECISION      XDREC  ( NWD )
            
      INTEGER               B
      INTEGER               BODYID
      INTEGER               CLBASE
      INTEGER               CLSIZE
      INTEGER               E
      INTEGER               FREE
      INTEGER               HANDLE
      INTEGER               NNHAN
      INTEGER               IREC   ( NWI )
      INTEGER               LASTLA ( 3 )
      INTEGER               LASTRC ( 3 )
      INTEGER               LASTWD ( 3 )
      INTEGER               NATBFF
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NREAD
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               NW
      INTEGER               OBFF
      INTEGER               RECNO
      INTEGER               REMAIN
      INTEGER               SURFID
      INTEGER               UNIT
      INTEGER               WORDNO
      INTEGER               XIREC  ( NWI )
      INTEGER               XNCOMC
      INTEGER               XNCOMR
      INTEGER               XNRSVC
      INTEGER               XNRSVR

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
      CALL TOPEN ( 'F_ZZDASGRC' )





C***********************************************************************
C
C     Setup
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup: create supporting kernels.' )

      IF ( EXISTS(PCK) ) THEN

         CALL DELFIL ( PCK )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

      CALL TSTPCK ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup: create tessellated ellipsoid DSK.' )

      BODYID = 499
      SURFID =   1
      FRNAME = 'IAU_MARS'
      
      NLON   = 40
      NLAT   = 20

      IF ( EXISTS(DSK) ) THEN

         CALL DELFIL ( DSK )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF


      CALL T_ELDS2Z ( BODYID, SURFID, FRNAME, NLON, NLAT, DSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Add a few comments to the DSK.
C
      CALL DASOPW ( DSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASAC ( HANDLE, 1, 'Comment line number one out of one.' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup: create non-native version of DSK.' )

C
C     Get the native format code and the code of the format
C     to which the file will be transformed.
C    
C
      CALL ZZDDHNFC ( NATBFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( NATBFF .EQ. LTLI3E ) THEN

         OBFF = BIGI3E

      ELSE IF ( NATBFF .EQ. BIGI3E ) THEN

         OBFF = LTLI3E

      ELSE

         CALL SETMSG ( 'Found unexpected native BFF code #.' )
         CALL ERRINT ( '#', NATBFF                           )
         CALL SIGERR ( 'SPICE(UNSUPPORTEDBFF)'               )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF
      
      
      IF ( EXISTS(NNDSK) ) THEN

         CALL DELFIL ( NNDSK )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

      CALL T_BINGO ( DSK, NNDSK, OBFF )
      CALL CHCKXC ( .FALSE., ' ', OK )


     

C***********************************************************************
C
C     Test ZZDASRFR
C
C***********************************************************************

C***********************************************************************
C
C     Normal cases
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Open native DSK for read access; read file '
     .//           'record.'                                    )


      CALL DASOPR ( DSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZDASRFR ( HANDLE, IDWORD, IFNAME, NRESVR, 
     .                NRESVC, NCOMR,  NCOMC          )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIDW   = 'DAS/DSK'
      XIFNAM = DSK
      XNRSVR = 0
      XNRSVC = 0
      XNCOMR = 1
      XNCOMC = 36

      CALL CHCKSC ( 'IDWORD', IDWORD, '=', XIDW,      OK )
      CALL CHCKSC ( 'IFNAME', IFNAME, '=', XIFNAM,    OK )
      CALL CHCKSI ( 'NRESVR', NRESVR, '=', XNRSVR, 0, OK )
      CALL CHCKSI ( 'NRESVC', NRESVC, '=', XNRSVC, 0, OK )
      CALL CHCKSI ( 'NCOMR',  NCOMR,  '=', XNCOMR, 0, OK )
      CALL CHCKSI ( 'NCOMC',  NCOMC,  '=', XNCOMC, 0, OK )
      
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Open NON-native DSK for read access; read file '
     .//           'record.'                                       )


      CALL DASOPR ( NNDSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZDASRFR ( HANDLE, IDWORD, IFNAME, NRESVR, 
     .                NRESVC, NCOMR,  NCOMC          )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Use the expected values from the previous test case.
C
      CALL CHCKSC ( 'IDWORD', IDWORD, '=', XIDW,      OK )
      CALL CHCKSC ( 'IFNAME', IFNAME, '=', XIFNAM,    OK )
      CALL CHCKSI ( 'NRESVR', NRESVR, '=', XNRSVR, 0, OK )
      CALL CHCKSI ( 'NRESVC', NRESVC, '=', XNRSVC, 0, OK )
      CALL CHCKSI ( 'NCOMR',  NCOMR,  '=', XNCOMR, 0, OK )
      CALL CHCKSI ( 'NCOMC',  NCOMC,  '=', XNCOMC, 0, OK )
      
      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************


C
C     We test only exception (2) of ZZDASRFR. The others should
C     be covered by tests of lower-level routines.
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDASRFR: Invalid file handle' )


      CALL ZZDASRFR ( -10000, IDWORD, IFNAME, NRESVR, 
     .                NRESVC, NCOMR,  NCOMC          )
      CALL CHCKXC ( .TRUE., 'SPICE(NOSUCHHANDLE)', OK )


C
C --- Case: ------------------------------------------------------
C

C     CALL TCASE ( 'ZZDASRFR: error reading file' )
C
C     This test has the undesirable side effect of leaving behind
C     a file with a system-dependent file name. We exclude this
C     case from standard TSPICE runs, but it can be run independently.
C
C
C     Open the file for read access, then delete it.
C
C
C     CALL DASOPR ( DSK, HANDLE )
C     CALL CHCKXC ( .FALSE., ' ', OK )
C
C     CALL KILFIL ( DSK )
C
C     CALL ZZDASRFR ( HANDLE, IDWORD, IFNAME, NRESVR,
C    .                NRESVC, NCOMR,  NCOMC          )
C     CALL CHCKXC ( .TRUE., 'SPICE(FILEREADFAILED)', OK )
C




C***********************************************************************
C
C     Test ZZDASGRD
C
C***********************************************************************

C***********************************************************************
C
C     Normal cases
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check d.p. records read from native DSK.' )
C
C     This is a consistency check only: DASRDD uses ZZDASGRD.
C     The next test case uses the independent routine DASIOD.
C

C
C     Get the DSK's file summary info.
C
      CALL DASOPR ( DSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC,
     .              FREE,   LASTLA, LASTRC, LASTWD       )

C
C     Let NW be the number of d.p. data words in the file.
C     
      NW = LASTLA( DPIX )

C
C     Read the d.p. data; compare with data that we fetch using
C     ZZDASGRD.
C
      
      REMAIN = NW
      B      = 1

      DO WHILE ( ( REMAIN .GT. 0 ) .AND. ( .NOT. FAILED() )  )
         
         NREAD = MIN( REMAIN, NWD )
         E     = B + NREAD - 1

         CALL DASRDD ( HANDLE, B, E, XDREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Get the number of the record containing address B.
C
         CALL DASA2L ( HANDLE, DPIX,  B,     CLBASE,
     .                 CLSIZE, RECNO, WORDNO        )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Read the record at index RECNO.
C
         CALL ZZDASGRD ( HANDLE, RECNO, DREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        We expect an exact match.
C
         LABEL = 'DREC@ record #'
         CALL REPMI ( LABEL, '#', RECNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, DREC, '=', XDREC, NREAD, 0.D0, OK )

C
C        Prepare for the next read, if any.
C
         REMAIN = REMAIN - NREAD
         B      = B      + NREAD

      END DO

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check d.p. records read from native DSK. '
     .//           'Use unit-oriented record reader to fetch '
     .//           'expected record.'                         )

C
C     Get the DSK's file summary info.
C
      CALL DASOPR ( DSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC,
     .              FREE,   LASTLA, LASTRC, LASTWD       )

C
C     Let NW be the number of d.p. data words in the file.
C     
      NW = LASTLA( DPIX )

C
C     Read the d.p. data; compare with data that we fetch using
C     ZZDASGRD.
C
      
      REMAIN = NW
      B      = 1

      DO WHILE ( ( REMAIN .GT. 0 ) .AND. ( .NOT. FAILED() )  )
         
         NREAD = MIN( REMAIN, NWD )
         E     = B + NREAD - 1

C
C        Get the number of the record containing address B.
C
         CALL DASA2L ( HANDLE, DPIX,  B,     CLBASE,
     .                 CLSIZE, RECNO, WORDNO        )
         CALL CHCKXC ( .FALSE., ' ', OK )


C
C        Get an unlocked logical unit for the DSK file.
C        Read the expected record.
C
         CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., UNIT )

         CALL DASIOD ( 'READ', UNIT, RECNO, XDREC  )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Read the record at index RECNO.
C
         CALL ZZDASGRD ( HANDLE, RECNO, DREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        We expect an exact match.
C
         LABEL = 'DREC@ record #'
         CALL REPMI ( LABEL, '#', RECNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, DREC, '=', XDREC, NREAD, 0.D0, OK )

C
C        Prepare for the next read, if any.
C
         REMAIN = REMAIN - NREAD
         B      = B      + NREAD

      END DO

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check d.p. records read from non-native DSK.' )

C
C     Get the DSK's file summary info.
C
      CALL DASOPR ( NNDSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC,
     .              FREE,   LASTLA, LASTRC, LASTWD       )

C
C     Let NW be the number of d.p. data words in the file.
C     
      NW = LASTLA( DPIX )

C
C     Read the d.p. data; compare with data that we fetch using
C     ZZDASGRD.
C
      
      REMAIN = NW
      B      = 1

      DO WHILE ( ( REMAIN .GT. 0 ) .AND. ( .NOT. FAILED() )  )
         
         NREAD = MIN( REMAIN, NWD )
         E     = B + NREAD - 1

         CALL DASRDD ( HANDLE, B, E, XDREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Get the number of the record containing address B.
C
         CALL DASA2L ( HANDLE, DPIX,  B,     CLBASE,
     .                 CLSIZE, RECNO, WORDNO        )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Read the record at index RECNO.
C
         CALL ZZDASGRD ( HANDLE, RECNO, DREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        We expect an exact match.
C
         LABEL = 'DREC@ record #'
         CALL REPMI ( LABEL, '#', RECNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, DREC, '=', XDREC, NREAD, 0.D0, OK )

C
C        Prepare for the next read, if any.
C
         REMAIN = REMAIN - NREAD
         B      = B      + NREAD

      END DO

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check d.p. records read from non-native DSK, '
     .//           'comparing directly to native data.'          )

C
C     Get the native DSK's file summary info.
C
      CALL DASOPR ( DSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC,
     .              FREE,   LASTLA, LASTRC, LASTWD       )


C
C     Open the non-native DSK for read access as well.
C
      CALL DASOPR ( NNDSK, NNHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Let NW be the number of d.p. data words in the file.
C     
      NW = LASTLA( DPIX )

C
C     Read the d.p. data from the native file; compare with data that
C     we fetch from the non-native file using ZZDASGRD.
C
      
      REMAIN = NW
      B      = 1

      DO WHILE ( ( REMAIN .GT. 0 ) .AND. ( .NOT. FAILED() )  )
         
         NREAD = MIN( REMAIN, NWD )
         E     = B + NREAD - 1

         CALL DASRDD ( HANDLE, B, E, XDREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Get the number of the record containing address B. We expect
C        this record number to be invariant across BFFs.
C
         CALL DASA2L ( HANDLE, DPIX,  B,     CLBASE,
     .                 CLSIZE, RECNO, WORDNO        )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Read the record at index RECNO in the non-native file.
C
         CALL ZZDASGRD ( NNHAN, RECNO, DREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        We expect an exact match.
C
         LABEL = 'DREC@ record #'
         CALL REPMI ( LABEL, '#', RECNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, DREC, '=', XDREC, NREAD, 0.D0, OK )

C
C        Prepare for the next read, if any.
C
         REMAIN = REMAIN - NREAD
         B      = B      + NREAD

      END DO

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DASCLS ( NNHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )


C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************


C
C     We test only exception (1) of ZZDASGRD. The others should
C     be covered by tests of lower-level routines.
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDASGRD: Invalid file handle' )


      CALL ZZDASGRD ( -10000, RECNO, DREC )
      CALL CHCKXC ( .TRUE., 'SPICE(NOSUCHHANDLE)', OK )


C***********************************************************************
C
C     Test ZZDASGRI
C
C***********************************************************************

C***********************************************************************
C
C     Normal cases
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check integer records read from native DSK.' )
C
C     This is a consistency check only: DASRDI uses ZZDASGRI.
C     The next test case uses the independent routine DASIOI.
C

C
C     Get the DSK's file summary info.
C
      CALL DASOPR ( DSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC,
     .              FREE,   LASTLA, LASTRC, LASTWD       )

C
C     Let NW be the number of integer data words in the file.
C     
      NW = LASTLA( INTIX )

C
C     Read the integer data; compare with data that we fetch using
C     ZZDASGRI.
C
      
      REMAIN = NW
      B      = 1

      DO WHILE ( ( REMAIN .GT. 0 ) .AND. ( .NOT. FAILED() )  )
         
         NREAD = MIN( REMAIN, NWI )
         E     = B + NREAD - 1

         CALL DASRDI ( HANDLE, B, E, XIREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Get the number of the record containing address B.
C
         CALL DASA2L ( HANDLE, INTIX, B,     CLBASE,
     .                 CLSIZE, RECNO, WORDNO        )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Read the record at index RECNO.
C
         CALL ZZDASGRI ( HANDLE, RECNO, IREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        We expect an exact match.
C
         LABEL = 'IREC@ record #'
         CALL REPMI ( LABEL, '#', RECNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( LABEL, IREC, '=', XIREC, NREAD, OK )

C
C        Prepare for the next read, if any.
C
         REMAIN = REMAIN - NREAD
         B      = B      + NREAD

      END DO

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )





C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check integer records read from native DSK.' 
     .//           'Use unit-oriented record reader to fetch '
     .//           'expected record.'                         )

C
C     Get the DSK's file summary info.
C
      CALL DASOPR ( DSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC,
     .              FREE,   LASTLA, LASTRC, LASTWD       )

C
C     Let NW be the number of integer data words in the file.
C     
      NW = LASTLA( INTIX )

C
C     Read the integer data; compare with data that we fetch using
C     ZZDASGRI.
C
      
      REMAIN = NW
      B      = 1

      DO WHILE ( ( REMAIN .GT. 0 ) .AND. ( .NOT. FAILED() )  )
         
         NREAD = MIN( REMAIN, NWI )
         E     = B + NREAD - 1


C
C        Get the number of the record containing address B.
C
         CALL DASA2L ( HANDLE, INTIX, B,     CLBASE,
     .                 CLSIZE, RECNO, WORDNO        )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Get an unlocked unit and read the expected record.
C
         CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., UNIT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DASIOI ( 'READ', UNIT, RECNO, XIREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Read the record at index RECNO.
C
         CALL ZZDASGRI ( HANDLE, RECNO, IREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        We expect an exact match.
C
         LABEL = 'IREC@ record #'
         CALL REPMI ( LABEL, '#', RECNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( LABEL, IREC, '=', XIREC, NREAD, OK )

C
C        Prepare for the next read, if any.
C
         REMAIN = REMAIN - NREAD
         B      = B      + NREAD

      END DO

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )





 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check integer records read from non-native DSK.' )

C
C     Get the DSK's file summary info.
C
      CALL DASOPR ( NNDSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC,
     .              FREE,   LASTLA, LASTRC, LASTWD       )

C
C     Let NW be the number of integer data words in the file.
C     
      NW = LASTLA( INTIX )

C
C     Read the integer data; compare with data that we fetch using
C     ZZDASGRI.
C
      
      REMAIN = NW
      B      = 1

      DO WHILE ( ( REMAIN .GT. 0 ) .AND. ( .NOT. FAILED() )  )
         
         NREAD = MIN( REMAIN, NWI )
         E     = B + NREAD - 1

         CALL DASRDI ( HANDLE, B, E, XIREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Get the number of the record containing address B.
C
         CALL DASA2L ( HANDLE, INTIX, B,     CLBASE,
     .                 CLSIZE, RECNO, WORDNO        )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Read the record at index RECNO.
C
         CALL ZZDASGRI ( HANDLE, RECNO, IREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        We expect an exact match.
C
         LABEL = 'IREC@ record #'
         CALL REPMI ( LABEL, '#', RECNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( LABEL, IREC, '=', XIREC, NREAD, OK )

C
C        Prepare for the next read, if any.
C
         REMAIN = REMAIN - NREAD
         B      = B      + NREAD

      END DO

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check integer records read from non-native DSK, '
     .//           'comparing directly to native data.'          )

C
C     Get the native DSK's file summary info.
C
      CALL DASOPR ( DSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC,
     .              FREE,   LASTLA, LASTRC, LASTWD       )


C
C     Open the non-native DSK for read access as well.
C
      CALL DASOPR ( NNDSK, NNHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Let NW be the number integer data words in the file.
C     
      NW = LASTLA( INTIX )

C
C     Read the integer data from the native file; compare with data that
C     we fetch from the non-native file using ZZDASGRI.
C
      
      REMAIN = NW
      B      = 1

      DO WHILE ( ( REMAIN .GT. 0 ) .AND. ( .NOT. FAILED() )  )
         
         NREAD = MIN( REMAIN, NWI )
         E     = B + NREAD - 1

         CALL DASRDI ( HANDLE, B, E, XIREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Get the number of the record containing address B. We expect
C        this record number to be invariant across BFFs.
C
         CALL DASA2L ( HANDLE, INTIX, B,     CLBASE,
     .                 CLSIZE, RECNO, WORDNO        )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Read the record at index RECNO in the non-native file.
C
         CALL ZZDASGRI ( NNHAN, RECNO, IREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        We expect an exact match.
C
         LABEL = 'IREC@ record #'
         CALL REPMI ( LABEL, '#', RECNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( LABEL, IREC, '=', XIREC, NREAD, OK )

C
C        Prepare for the next read, if any.
C
         REMAIN = REMAIN - NREAD
         B      = B      + NREAD

      END DO

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DASCLS ( NNHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )



C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************


C
C     We test only exception (1) of ZZDASGRI. The others should
C     be covered by tests of lower-level routines.
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDASGRI: Invalid file handle' )


      CALL ZZDASGRI ( -10000, RECNO, IREC )
      CALL CHCKXC ( .TRUE., 'SPICE(NOSUCHHANDLE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up: delete kernels.' )
  
      IF ( EXISTS(PCK) ) THEN
         CALL DELFIL ( PCK )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF
         
      CALL UNLOAD ( DSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( NNDSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( NNDSK )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


