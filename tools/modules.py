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
