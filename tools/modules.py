"""
Which SPICELIB routines go into which PEPPER module (used by
tools/modularize.py). Modules are listed in dependency order: a module's
routines may use only modules listed before it.

Steps to move the routines of a new module (MODULE = e.g. pepper_cells):

    python tools/modularize.py MODULE
    python tools/add_intents.py src/MODULE.f90
    pixi run python tools/mark_pure.py src/MODULE.f90
    python tools/document_procedures.py --remove-brief-io \
        src/MODULE.f90 src/legacy/pepper_legacy_<name>.f90
    python tools/modernize_syntax.py src
    python tools/wrap_docs.py src
    pixi run tspice

Steps for a context module (CONTEXT_MODULES; intents, documentation and
the Brief_I/O removal are done by modularize_ctx.py itself):

    python tools/data_to_parameter.py src/spicelib/<file>  (read-only tables)
    python tools/modularize_ctx.py --dry-run MODULE     (checks the calls)
    python tools/modularize_ctx.py MODULE
    python tools/modernize_syntax.py src
    python tools/wrap_docs.py src
    pixi run tspice

Constants computed on the first call (a saved FIRST flag) are rewritten as
parameters by hand before the move.
"""

# Constant functions of SPICELIB that are named constants in
# pepper_constants: name -> kind ("wp" real, "ip" integer).
CONSTANTS = {
    **{n: "wp" for n in "b1900 b1950 clight dpmax dpmin dpr halfpi j1900 j1950 "
                        "j2000 j2100 jyear pi rpd spd twopi tyear".split()},
    **{n: "ip" for n in "intmax intmin".split()},
}

MODULES = {
    "pepper_constants": sorted(CONSTANTS),
    "pepper_arrays": """
        alltru bschoc bschoi bsrchc bsrchd bsrchi clearc cleard cleari fillc
        filld filli isordv isrchc isrchd isrchi lstcld lstcli lstlec lstled
        lstlei lstltc lstltd lstlti maxac maxad maxai minac minad minai movec
        moved movei notru orderc orderd orderi prodad prodai reordc reordd
        reordi reordl rmdupc rmdupd rmdupi sameai shellc shelld shelli somfls
        somtru sumad sumai swapc swapd swapi
        touchc touchd touchi touchl
        """.split(),
    "pepper_math": """
        approx axisar azlrec brcktd brckti chbder chbint chbval cyllat cylrec
        cylsph dcbrt det drdcyl drdlat drdsph ducrss dvcrss dvdot dvhat dvnorm
        el2cgv even exact frame gcd ident insang invert latcyl latrec latsph
        mequ mequg mtxm mtxmg mtxv mtxvg mxm mxmg mxmt mxmtg mxv mxvg odd
        opsgnd opsgni pl2nvc pl2nvp pl2psv pltexp pltnrm polyds q2m qdq2av qxq
        radrec rav2xf recazl reccyl reclat recrad recsph sharpr smsgnd smsgni
        sphcyl sphlat sphrec trace traceg ucrss unorm unormg vadd vaddg vcrss
        vdist vdistg vdot vdotg vequ vequg vhat vhatg vhatip vlcom vlcom3
        vlcomg vminug vminus vnorm vnormg vpack vperp vproj vprojg vrel vrelg
        vrotv vscl vsclg vsclip vsep vsepg vsub vsubg vtmv vtmvg vupack vzero
        vzerog xf2rav xpose xposeg xpsgip zzmsxf zzrxr
        """.split(),
    "pepper_strings": """
        astrip bedec beint benum beuns cmprss cpos cposr eqstr esrchc fndnwd
        frstnb frstnp frstpc kxtrct lastnb lastpc lbuild lcase ljucrs ljust
        lparse lparsm ltrim lxqstr matchw nblen nbwid ncpos ncposr nextwd nthwd
        parsqs pcwid pos posr prefix quote replch replwd repmc rjust rtrim
        samch samsub scanrj shiftl shiftr sigdgt suffix ucase wdcnt wdindx
        zzinssub zzrepsub intstr dpstr eqchr nechr samchi samsbi matchi
        nparsd nparsi lx4uns
        """.split(),
    "pepper_errors": ["expln"],
}

# Routines that need the context (they signal errors): moved by
# tools/modularize_ctx.py into submodules of pepper_instance. In dependency
# order, like MODULES.
CONTEXT_MODULES = {
    "pepper_cells": """
        chbase enchar dechar excess
        cardc cardd cardi sizec sized sizei scardc scardd scardi
        ssizec ssized ssizei validc validd validi
        appndc appndd appndi copyc copyd copyi insrtc insrtd insrti
        removc removd removi elemc elemd elemi ordc ordd ordi setc setd seti
        unionc uniond unioni interc interd interi diffc diffd diffi
        sdiffc sdiffd sdiffi
        zzwninsd wncard wncomd wncond wndifd wnelmd wnexpd wnextd wnfetd
        wnfild wnfltd wnincd wninsd wnintd wnreld wnsumd wnunid wnvald
        """.split(),
    "pepper_numerics": """
        dacosh dacosn dasine datanh dazldr dcyldr dlatdr drdazl drdgeo dsphdr
        eqncpv edpnt ednmpt elltof hrmesp hrmint lgresp lgrind lgrint chbigr
        inelpl inrypl invort invstm isrot kepleq kpsolv m2q nplnpt npsgpt
        nvc2pl nvp2pl psv2pl pltar pltnp pltvol qderiv raxisa rquad rmaind
        rmaini vprjp vprjpi stelab stlabx xposbl
        """.split(),
    "pepper_array_ops": """
        cyacip cyadip cyaiip cyclac cyclad cyclai inslac inslad inslai
        remlac remlad remlai swapac swapad swapai packac packad packai
        lnkan lnkfsl lnkhl lnkila lnkilb lnkini lnknfn lnknxt lnkprv lnksiz
        lnktl lnkxsl
        """.split(),
    "pepper_text": """
        cyclec repmd repmi repsub remsub inssub shiftc chckid countc hx2int
        locati locln
        """.split(),
    "pepper_pool": """
        zzctr ioerr zzcln rdtext rdker zztknerr zzmkpc zzrept tcheck texpyr
        zztpats tpartv zztime tparse zzpini zzrvar zzrvbf zzgapool zzgpnm
        zznwpool pool
        """.split(),
    "pepper_bodies": """
        zzidmap zzhashi zzhsc zzhsi zzbodini zzbodker zzbodblt zzbodtrn
        bodn2c bodc2n bods2c bodc2s boddef bodfnd zzbods2c zzbodvcd bodvar
        bodvcd bodvrd
        """.split(),
    "pepper_files": """
        zzplatfm zzpltchk zzftpstr zzftpchk zzrbrkst zzddhini zzgetbff
        zzddhgsd zzddhmnm zzddhppf idw2at zzxlatei zzddhivf zzddhf2h zzddhrcm
        zzddhgtu zzddhrmu zzddhclu zzddhnfc zzddhman
        """.split(),
    "pepper_daf": """
        rdenci
        hx2dp
        rdencd
        txtopn
        txtopr
        wrenci
        int2hx
        dp2hx
        wrencd
        zzdafgfr zzdafnfr zzdafgdr zzdafgsr dafah dafrwa dafps dafrwd dafgda
        zzxlated dafrda dafrfr dafwfr dafwda dafarr dafrcr dafwcr dafra dafrrr dafac
        dafdc dafec dafana daffa dafbt dafa2b dafb2a dafb2t daft2b daftb
        """.split(),
}

# Hand-written stateless modules (not generated from SPICELIB files) whose
# procedures moved code may call.
HANDWRITTEN = {
    "pepper_hash": ["name_hash"],
}

# Umbrella routines (shared saved variables and ENTRY points) moved by
# tools/modularize_ctx.py: each entry point becomes a module procedure. The
# "state" variables are the saved variables that really persist between
# calls; they become the components of a derived type (in a generated module
# pepper_state_<name>) held in pepper_spice as the allocatable component
# "component" ("where": "context" holds it in pepper_context instead, for
# state of a computation in progress). "state": "all" makes every local
# variable state (exactly SPICELIB's behavior with a bare SAVE). All other
# saved variables are temporaries and become plain local variables. An
# umbrella is listed by its name in CONTEXT_MODULES. "state": "saved" takes the
# variables named in SAVE statements or initialized by DATA; it also works for
# routines without ENTRY points (e.g. with a saved cache).
UMBRELLAS = {
    # Saved caches of the last name/code translated: per context.
    "bodvrd": {"state": "saved", "component": "bodvrd_cache", "where": "context", "entries": ['bodvrd']},
    # The handle manager: the file table belongs to the instance; the units
    # (and the tables of format names built on the first call) to the context,
    # so that each thread opens and recycles its own units.
    "zzddhman": {"state": "nft ftabs ftamh ftarc ftbff fthan ftnam ftrtm ftmnm next".split(),
                 "component": "zzddhman",
                 "context_state": """first opnfst stramh strarc strbff natbff supbff
                                     numsup nut utcst uthan utlck utlun reqcnt""".split(),
                 "context_component": "zzddhman_units",
                 "docs": {"nft": "number of files in the file table",
                          "ftabs": "absolute value of each file's handle",
                          "ftamh": "access method of each file",
                          "ftarc": "architecture (DAF, DAS) of each file",
                          "ftbff": "binary file format of each file",
                          "fthan": "handle of each file",
                          "ftnam": "name of each file",
                          "ftrtm": "length of each file name",
                          "ftmnm": "unique number (from the file's attributes) of each file",
                          "next": "the next handle number (handles are never reused)",
                          "first": "the tables of names have not been built yet",
                          "opnfst": "no file has been opened yet (platform check)",
                          "stramh": "names of the access methods",
                          "strarc": "names of the architectures",
                          "strbff": "names of the binary file formats",
                          "natbff": "the native binary file format",
                          "supbff": "the supported binary file formats",
                          "numsup": "number of supported binary file formats",
                          "nut": "number of entries in the unit table",
                          "utcst": "cost (time of the last request) of each unit",
                          "uthan": "handle of the file connected to each unit",
                          "utlck": "the unit is locked to its file",
                          "utlun": "the units",
                          "reqcnt": "request counter (for the costs)"},
                 "entries": ['zzddhopn', 'zzddhcls', 'zzddhhlu', 'zzddhunl', 'zzddhnfo', 'zzddhisn', 'zzddhfnh', 'zzddhluh']},
    # First-call tables of format and platform information: per context.
    **{n: {"state": "saved", "component": n, "where": "context", "entries": [n]} for n in
       "zzplatfm zzftpstr zzftpchk zzgetbff zzddhgsd zzddhmnm zzddhppf zzxlatei zzddhnfc".split()},
    # DAF: the file table belongs to the instance; searches and arrays being
    # written, record buffers and first-call tables to the context.
    "dafah": {"state": "nft fthan ftlnk ftnd ftni fhlist first".split(), "component": "dafah",
              "docs": {"nft": "number of DAFs in the file table",
                       "fthan": "handle of each DAF",
                       "ftlnk": "number of links (opens) of each DAF",
                       "ftnd": "number of double precision components of the summaries",
                       "ftni": "number of integer components of the summaries",
                       "fhlist": "set of the handles of the open DAFs",
                       "first": "the handle set has not been initialized yet"},
              "entries": ['dafopr', 'dafopw', 'dafonw', 'dafopn', 'dafcls', 'dafhsf', 'dafhlu', 'dafluh', 'dafhfn', 'daffnh', 'dafhof', 'dafsih']},
    "daffa": {"state": "all", "component": "daffa", "where": "context", "entries": ['dafbfs', 'daffna', 'dafbbs', 'daffpa', 'dafgs', 'dafgn', 'dafgh', 'dafrs', 'dafrn', 'dafws', 'dafcs']},
    "dafana": {"state": "all", "component": "dafana", "where": "context", "entries": ['dafbna', 'dafada', 'dafena', 'dafcad']},
    "dafrwd": {"state": "saved", "component": "dafrwd", "where": "context", "entries": ['dafgdr', 'dafgsr', 'dafrdr', 'dafwdr', 'dafnrr']},
    "dafps": {"state": [], "component": None, "entries": ['dafps', 'dafus']},
    "dafrwa": {"state": [], "component": None, "entries": ['dafrwa', 'dafarw']},
    # (component <name>_state: a component may not have the name of a
    # type-bound procedure)
    **{n: {"state": "saved", "component": n + "_state", "where": "context", "entries": [n]} for n in
       "dafec dafwda zzdafgdr zzdafgsr zzdafgfr zzdafnfr zzxlated hx2dp".split()},
    "zzhsc": {"state": [], "component": None, "entries": ['zzhscini', 'zzhscadd', 'zzhscchk', 'zzhscavl', 'zzhscinf']},
    "zzhsi": {"state": [], "component": None, "entries": ['zzhsiini', 'zzhsiadd', 'zzhsichk', 'zzhsiavl', 'zzhsiinf']},
    # Built-in body names and codes (tables built on the first call).
    "zzbodblt": {"state": "all", "component": "zzbodblt", "where": "context", "entries": ['zzbodget', 'zzbodlst']},
    # Body name/code tables derived from the pool: a cache, per context.
    "zzbodtrn": {"state": "all", "component": "zzbodtrn", "where": "context", "entries": ['zzbodn2c', 'zzbodc2n', 'zzboddef', 'zzbodkik', 'zzbodrst', 'zzbctrck']},
    "zzctr": {"state": [], "component": None, "entries": ['zzctruin', 'zzctrsin', 'zzctrinc', 'zzctrchk']},
    "tcheck": {"state": ["dochck"], "component": "tcheck", "entries": ['tcheck', 'tparch', 'tchckd'], "docs": {'dochck': 'check the components of parsed times (set by TPARCH)'}},
    "texpyr": {"state": ["centry", "lbound"], "component": "texpyr", "entries": ['texpyr', 'tsetyr'], "docs": {'centry': 'the century assumed for two-digit years', 'lbound': 'the lower bound of the 100-year window for two-digit years'}},
    # Saves the tokens of the string being parsed between calls: per context.
    "zztime": {"state": "all", "component": "zztime", "where": "context", "entries": ['zzcmbt', 'zzgrep', 'zzispt', 'zzist', 'zznote', 'zzremt', 'zzsubt', 'zztokns', 'zzunpck', 'zzvalt']},
    "rdtext": {"state": "units n lstfil lstunt index".split(), "component": "rdtext", "entries": ['rdtext', 'cltext'], "docs": {'units': 'units of the files open for reading', 'n': 'number of files open', 'lstfil': 'the file read last', 'lstunt': 'the unit of the file read last', 'index': 'index in units of the file read last'}},
    "rdker": {"state": "file linnum status frstim begdat begtxt".split(), "component": "rdker", "entries": ['rdknew', 'rdkdat', 'rdklin'], "docs": {'file': 'the text kernel being read', 'linnum': 'number of the last line read', 'status': 'reading text, data, or at the end of the file', 'frstim': 'no kernel has been opened yet', 'begdat': 'the begin-data marker', 'begtxt': 'the begin-text marker'}},
    "pool": {"state": """pnames namlst nmpool datlst chpool dppool chvals dpvals
                         wtvars wtpool wtagnt wtptrs active agents notify first subctr
                         begdat begtxt""".split(), "component": "pool", "entries": ['clpool', 'ldpool', 'rtpool', 'expool', 'wrpool', 'swpool', 'cvpool', 'gcpool', 'gdpool', 'gipool', 'dtpool', 'pcpool', 'pdpool', 'pipool', 'lmpool', 'szpool', 'dvpool', 'gnpool', 'dwpool', 'zzvupool', 'zzpctrck'], "docs": {'pnames': 'names of the kernel pool variables', 'namlst': 'heads of the hash collision lists (by hash value of the name)', 'nmpool': 'linked-list pool of the name collision lists', 'datlst': 'heads of the value lists of the variables (positive: numeric, negative: character)', 'chpool': 'linked-list pool of the character values', 'dppool': 'linked-list pool of the numeric values', 'chvals': 'character values', 'dpvals': 'numeric values', 'wtvars': 'watcher table: the watched variables (a set)', 'wtpool': 'linked-list pool of the watcher table', 'wtagnt': 'watcher table: the agents watching each variable', 'wtptrs': 'watcher table: pointers into wtpool for each watched variable', 'active': 'set of the agents that have watchers', 'agents': 'work set of agents', 'notify': 'set of the agents to notify of an update', 'first': 'the pool has not been initialized yet', 'subctr': 'pool state counter (changes when the pool changes)', 'begdat': 'the begin-data marker of text kernels', 'begtxt': 'the begin-text marker of text kernels'}},
}

# Legacy routines (still in src/spicelib) that moved code may call for now,
# with the reason. Remove an entry when the routine moves.
ALLOWED_LEGACY = {
    "wrline": "writes a line to a device or file (SPICELIB's error output; it moves "
              "with the error output, phase 7)",
}
