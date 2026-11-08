C$Procedure F_DDHNFO ( ZZDDHNFO Test Family )
 
      SUBROUTINE F_DDHNFO ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the ZZDDHNFO
C     routine.
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
 
      INCLUDE              'zzddhman.inc'
 
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
C$ Files
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
C$ Particulars
C
C     This routine exercises ZZDDHNFO's logic.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    TSPICE Version 2.0.0, 07-AUG-2002 (FST)
C
C        Updated module as the result of changes to the handle
C        manager interface, ZZDDHCLS.
C
C-    TSPICE Version 1.0.0, 05-SEP-2001 (FST)
C
C-&
 
C
C     Local Parameters
C
C     NUMFIL is the maximum number of different possible types of files
C     that this system may open in the ZZDDHMAN interface.  It is
C     computed based on the following:
C
C        There are NUMARC architectures.  Each method is valid for
C        either architecture.
C
C        All non-native files may only be opened for 'READ' access.
C
      INTEGER               NUMFIL
      PARAMETER           ( NUMFIL = NUMARC*(NUMAMH+NUMBFF) )
 
C
C     Test Table
C
      CHARACTER*(FILEN)     TBLFNM ( NUMFIL )
      INTEGER               TBLAMH ( NUMFIL )
      INTEGER               TBLARC ( NUMFIL )
      INTEGER               TBLBFF ( NUMFIL )
      INTEGER               TBLHAN ( NUMFIL )
 
C
C     Local Variables
C
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(FILEN)     FNMPAT
      CHARACTER*(STRSIZ)    STRAMH ( NUMAMH )
      CHARACTER*(STRSIZ)    STRARC ( NUMARC )
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
      CHARACTER*(80)        TCSTR
      CHARACTER*(FILEN)     TSTSTR
 
      INTEGER               FILCNT
      INTEGER               I
      INTEGER               INTAMH
      INTEGER               INTARC
      INTEGER               INTBFF
      INTEGER               IOSTAT
      INTEGER               J
      INTEGER               K
      INTEGER               NATBFF
      INTEGER               NUMSUP
      INTEGER               SUPBFF ( NUMARC )
      INTEGER               UNIT
 
      LOGICAL               FOUND
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_DDHNFO' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'F_DDHNFO Initialization' )
 
C
C     Set the filename pattern we will use.
C
      FNMPAT = 'test#.fil'
 
C
C     Fetch some initialization data.
C
      CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP, STRAMH, STRARC, STRBFF )
 
C
C     Now create entries in the test table for each of the native
C     binary file format files of each architecture and access method.
C
      DO I = 1, ( NUMARC * NUMAMH )
 
         CALL REPMI ( FNMPAT, '#', I, TBLFNM(I) )
         TBLARC(I) = MOD(I, NUMARC) + 1
         TBLAMH(I) = (I-1)/NUMARC+1
         TBLBFF(I) = NATBFF
 
      END DO
 
C
C     Create entries in the test table for each of the non-native
C     binary file format files of each architecture with 'READ' access.
C
      I = ( NUMARC * NUMAMH )
 
      DO J = 1, NUMSUP
 
C
C        We are going to ignore the native binary file format, since
C        we addressed it in the preceding loop.
C
         IF ( SUPBFF(J) .NE. NATBFF ) THEN
 
C
C           Loop over every possible architecture, preparing the
C           list of files for 'READ' access.
C
            DO K = 1, NUMARC
 
               I = I + 1
 
               CALL REPMI ( FNMPAT, '#', I, TBLFNM(I) )
               TBLARC(I) = MOD(I, NUMARC) + 1
               TBLAMH(I) = READ
               TBLBFF(I) = SUPBFF(J)
 
            END DO
 
         END IF
 
      END DO
 
C
C     Store the number of files we placed into the handle list.
C
      FILCNT = I
 
C
C     Now create the files.
C
      DO I = 1, FILCNT
 
C
C        Only create files for access methods that are not 'NEW'
C        or 'SCRATCH'.
C
         IF (       ( TBLAMH(I) .NE. NEW    )
     .        .AND. ( TBLAMH(I) .NE. SCRTCH )  ) THEN
 
C
C           Create the file.
C
            CALL T_CPTFIL ( TBLFNM(I),
     .                      TBLARC(I),
     .                      2,
     .                      STRBFF(TBLBFF(I)),
     .                      'ABCD',
     .                      'EFGH',
     .                      'IJKL',
     .                      .TRUE.,
     .                      .FALSE.,
     .                      ' '      )
 
         END IF
 
C
C        Open the file into the handle manager.
C
         CALL ZZDDHOPN ( TBLFNM(I),
     .                   STRAMH(TBLAMH(I)),
     .                   STRARC(TBLARC(I)),
     .                   TBLHAN(I)          )
 
      END DO
 
C
C     Now that we have finished all of the initialization stuff,
C     Perform the checks on each of the files in the test table.
C
      DO I = 1, FILCNT
 
C
C        Treat each file as an individual case.  Generate the
C        TCASE string automatically from the data in the table.
C
         TCSTR = 'Exercise - # # # Nominal Logic'
 
         CALL REPMC ( TCSTR, '#', STRAMH(TBLAMH(I)), TCSTR )
         CALL REPMC ( TCSTR, '#', STRBFF(TBLBFF(I)), TCSTR )
         CALL REPMC ( TCSTR, '#', STRARC(TBLARC(I)), TCSTR )
 
         CALL TCASE ( TCSTR )
 
C
C        Now setup the inputs and outputs.
C
         FNAME  = ' '
         INTARC = 0
         INTBFF = 0
         INTAMH = 0
         FOUND  = .FALSE.
 
C
C        Invoke the module.
C
         CALL ZZDDHNFO ( TBLHAN(I),
     .                   FNAME,
     .                   INTARC,
     .                   INTBFF,
     .                   INTAMH,
     .                   FOUND      )
 
C
C        Check for the absence of an exception.
C
         CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C        Check outputs.
C
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
 
C
C        Handle the case of scratch filenames separately.
C        Since their names are constructed automatically.
C
         IF ( TBLAMH(I) .EQ. SCRTCH ) THEN
 
C
C           INQUIRE on the UNIT associated with the scratch
C           handle.
C
            CALL ZZDDHHLU ( TBLHAN(I), STRARC(TBLARC(I)),
     .                      .FALSE.,   UNIT               )
 
C
C           Set the default value of the filename, in case the
C           INQUIRE does not change it.
C
            TSTSTR = '# SCRATCH FILE'
            CALL REPMC ( TSTSTR, '#', STRARC(TBLARC(I)), TSTSTR )
 
            INQUIRE ( UNIT = UNIT,
     .                NAME = TSTSTR,
     .                IOSTAT = IOSTAT )
 
C
C           Check the IOSTAT from the INQUIRE.
C
            CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
         ELSE
 
            TSTSTR = TBLFNM(I)
 
         END IF
 
         CALL CHCKSC ( 'FNAME',  FNAME,  '=', TSTSTR,    OK )
 
         CALL CHCKSI ( 'INTARC', INTARC, '=', TBLARC(I), 0, OK )
         CALL CHCKSI ( 'INTBFF', INTBFF, '=', TBLBFF(I), 0, OK )
         CALL CHCKSI ( 'INTAMH', INTAMH, '=', TBLAMH(I), 0, OK )
 
      END DO
 
C
C     Check for the absence of a rogue exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Exercise Handle Not Found Exception Logic' )
 
C
C     Setup inputs and outputs.
C
      FNAME  = 'BOGUS FILENAME'
      INTARC = 1
      INTBFF = 1
      INTAMH = 1
      FOUND  = .TRUE.
 
C
C     Invoke the module on the zero-valued handle.
C
      CALL ZZDDHNFO ( 0, FNAME, INTARC, INTBFF, INTAMH, FOUND )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
 
      CALL CHCKSC ( 'FNAME', FNAME, '=', ' ', OK )
 
      CALL CHCKSI ( 'INTARC', INTARC, '=', 0, 0, OK )
      CALL CHCKSI ( 'INTBFF', INTBFF, '=', 0, 0, OK )
      CALL CHCKSI ( 'INTAMH', INTAMH, '=', 0, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Exercise Handle Opposite Sign Exception Logic' )
 
C
C     Setup inputs and outputs.
C
      FNAME  = 'BOGUS FILENAME'
      INTARC = 1
      INTBFF = 1
      INTAMH = 1
      FOUND  = .TRUE.
 
C
C     Invoke the module on the negative of an existing, in-use handle.
C
         CALL ZZDDHNFO ( -TBLHAN(1),
     .                   FNAME,
     .                   INTARC,
     .                   INTBFF,
     .                   INTAMH,
     .                   FOUND       )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
 
      CALL CHCKSC ( 'FNAME', FNAME, '=', ' ', OK )
 
      CALL CHCKSI ( 'INTARC', INTARC, '=', 0, 0, OK )
      CALL CHCKSI ( 'INTBFF', INTBFF, '=', 0, 0, OK )
      CALL CHCKSI ( 'INTAMH', INTAMH, '=', 0, 0, OK )
 
C
C ----------------------------------------------------------------
C
C     Now clean up.
C
      DO I = 1, FILCNT
         CALL ZZDDHCLS ( TBLHAN(I), STRARC(TBLARC(I)), .FALSE. )
         CALL KILFIL   ( TBLFNM(I) )
      END DO
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
