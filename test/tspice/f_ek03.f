C$Procedure F_EK03 ( EK tests, subset 3 )
 
      SUBROUTINE F_EK03 ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB EK routines, subset 3.
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
  
      INCLUDE 'ekattdsc.inc'
      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekglimit.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ektnamsz.inc'
      INCLUDE 'ektype.inc'
      INCLUDE 'errhnd.inc'

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
C     This routine tests the following subset of the EK routines:
C   
C        EKCCNT
C        EKCII
C        EKNTAB
C        EKTNAM
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
C-    TSPICE Version 1.0.1, 20-FEB-2009 (NJB)
C
C        Corrected minor header typo. Re-ordered header sections.
C
C-    TSPICE Version 1.0.0, 12-SEP-2005 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               EKNSEG

C
C     Local Parameters
C
      CHARACTER*(*)         EK1
      PARAMETER           ( EK1    = 'test1.ek' )

      CHARACTER*(*)         EK2
      PARAMETER           ( EK2    = 'test2.ek' )

      INTEGER               DECLEN
      PARAMETER           ( DECLEN = 200 )

      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MAXENT
      PARAMETER           ( MAXENT = 20 )

      INTEGER               MAXROW
      PARAMETER           ( MAXROW = 20 )

      INTEGER               CVALSZ
      PARAMETER           ( CVALSZ = MAXSTR )

      INTEGER               NTABS
      PARAMETER           ( NTABS  = 6 )

      INTEGER               MAXVAL
      PARAMETER           ( MAXVAL = MAXENT )

C
C     Local Variables
C
      CHARACTER*(CNAMSZ)    CNAMES ( MXCLSG )
      CHARACTER*(CNAMSZ)    COLNAM
      CHARACTER*(DECLEN)    DECLS  ( MXCLSG )
      CHARACTER*(LMSGLN)    MSG
      CHARACTER*(TNAMSZ)    TABNAM

      INTEGER               ATTDSC ( ADSCSZ )
      INTEGER               DIMS   ( MXCLSG )
      INTEGER               DTYPES ( MXCLSG )
      INTEGER               CCLASS ( MXCLSG )
      INTEGER               FILENO
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               NCOLS
      INTEGER               NROWS
      INTEGER               NSEG
      INTEGER               NTAB
      INTEGER               SEGTYP
      INTEGER               STLENS ( MXCLSG )
      INTEGER               TAB
      INTEGER               XNCOLS

      LOGICAL               BOOL
      LOGICAL               INDEXD ( MXCLSG )
      LOGICAL               NULLOK ( MXCLSG )

      
C
C     Open the test family.
C
      CALL TOPEN ( 'F_EK03' )

      CALL TCASE ( 'Setup:  create an EK.' )
C
C     Create an EK.
C
      FILENO = 1

      CALL TSTEK  ( EK1, FILENO, MAXROW, .FALSE., HANDLE )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL TCASE ( 'Test EKNTAB: make sure the number of tables '//
     .             'matches the number of segments in EK1.'        )
C
C     Find out how many segments are in the EK.  By the specification
C     of TSTEK, there's one segment per table.
C
      CALL EKOPR  ( EK1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NSEG = EKNSEG ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the EK.
C
      CALL EKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load the EK into the query system.
C
      CALL FURNSH ( EK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make a copy of EK1 and load it as well. 
C     
      CALL TSTEK  ( EK2, FILENO, MAXROW, .FALSE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( EK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the number of loaded tables.
C
      CALL EKNTAB ( NTAB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'Number of loaded tables NTAB', 
     .              NTAB, '=', NSEG, 0, OK           )


C
C     Here ntab is the number of tables looked up via EKNTAB.
C
      DO TAB = 1, NTAB
C
C        Start a new test case.
C
         MSG = 'Testing EKTNAM, EKCCNT, EKCII for table #'
         CALL REPMI ( MSG, '#', TAB, MSG )

         CALL TCASE ( MSG )

C
C        Get the table name.
C        
         CALL EKTNAM ( TAB, TABNAM )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Look up the schema for this table.
C
         CALL TSTSCH ( TABNAM, MAXROW, SEGTYP, NROWS,  XNCOLS,
     .                 CNAMES, CCLASS, DTYPES, STLENS, DIMS,
     .                 INDEXD, NULLOK, DECLS                   )

         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Get the column count.
C
         CALL EKCCNT ( TABNAM, NCOLS )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'NCOLS from EKCCNT', 
     .                  NCOLS, '=', XNCOLS, 0, OK )

C
C        For each column in the current table, look up the column's
C        attributes.  The attribute block index parameters are defined
C        in the include file ekattdsc.inc.
C
         DO I = 1, NCOLS
C
C           Look up the attribute information for the ith column.
C
            CALL EKCII ( TABNAM, I, COLNAM, ATTDSC )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Check the column name.
C
            CALL CHCKSC ( 'COLNAM', COLNAM, '=', CNAMES(I), OK )

C
C           Check the current column's class.
C
            CALL CHCKSI ( 'COLNAM class', 
     .                     ATTDSC(ATTCLS), '=', CCLASS(I), 0, OK )

C
C           Check the current column's data type.
C
            CALL CHCKSI ( 'COLNAM data type', 
     .                     ATTDSC(ATTTYP), '=', DTYPES(I), 0, OK )

C
C           If the data type is character, check the string length.
C           
            IF ( ATTDSC(ATTTYP) .EQ. CHR ) THEN

                CALL CHCKSI ( 'COLNAM string length', 
     .                        ATTDSC(ATTLEN), '=', STLENS(I), 0, OK )
            END IF

C
C           Check the current column's entry size.
C
            CALL CHCKSI ( 'COLNAM entry size', 
     .                     ATTDSC(ATTSIZ), '=', DIMS(I), 0, OK )


C
C           Check the current column's index flag.
C
            BOOL = ATTDSC(ATTIDX) .EQ. ITRUE

            CALL CHCKSL ( 'COLNAM index flag', BOOL, INDEXD(I), OK )

C
C           Check the current column's null ok flag.
C
            BOOL = ATTDSC(ATTNFL) .EQ. ITRUE

            CALL CHCKSL ( 'COLNAM null ok flag', BOOL, NULLOK(I), OK )

         END DO

C
C        We're done with the current column.
C        

      END DO
C
C     We're done with the current table.
C     


C
C     Unload the EKs.  The TSPICE system will delete the files.
C
      CALL TCASE  ( 'Unload EKs from query system.' )

      CALL UNLOAD ( EK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( EK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


