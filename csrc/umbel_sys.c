/* csrc/umbel_sys.c — minimal POSIX durability & locking bridge for Lua 5.4.
 *
 * Exposes fsync, fdatasync, sync_dir, flock, and pid to Fennel/Lua so umbel
 * has real power-loss durability and multi-process file locking without
 * pulling in external Lua rock dependencies.
 */

#define _DEFAULT_SOURCE
#define _POSIX_C_SOURCE 200809L

#include <errno.h>
#include <fcntl.h>
#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <unistd.h>

#include <lauxlib.h>
#include <lua.h>

static FILE *check_file(lua_State *L, int idx) {
    luaL_Stream *s = (luaL_Stream *)luaL_checkudata(L, idx, LUA_FILEHANDLE);
    if (s->closef == NULL || s->f == NULL) {
        luaL_error(L, "umbel.sys: attempt to use a closed file");
    }
    return s->f;
}

static int l_fsync(lua_State *L) {
    FILE *f = check_file(L, 1);
    if (fflush(f) != 0) {
        return luaL_error(L, "umbel.sys.fsync fflush failed: %s", strerror(errno));
    }
    if (fsync(fileno(f)) != 0) {
        return luaL_error(L, "umbel.sys.fsync failed: %s", strerror(errno));
    }
    lua_pushboolean(L, 1);
    return 1;
}

static int l_fdatasync(lua_State *L) {
    FILE *f = check_file(L, 1);
    if (fflush(f) != 0) {
        return luaL_error(L, "umbel.sys.fdatasync fflush failed: %s", strerror(errno));
    }
    if (fdatasync(fileno(f)) != 0) {
        return luaL_error(L, "umbel.sys.fdatasync failed: %s", strerror(errno));
    }
    lua_pushboolean(L, 1);
    return 1;
}

static int l_sync_dir(lua_State *L) {
    const char *path = luaL_checkstring(L, 1);
    struct stat st;
    const char *dir_path = path;
    char *copy = NULL;

    if (stat(path, &st) != 0 || !S_ISDIR(st.st_mode)) {
        copy = strdup(path);
        if (copy == NULL) {
            return luaL_error(L, "umbel.sys.sync_dir: out of memory");
        }
        dir_path = dirname(copy);
    }

    int fd = open(dir_path, O_RDONLY | O_DIRECTORY);
    if (fd < 0) {
        int err = errno;
        free(copy);
        return luaL_error(L, "umbel.sys.sync_dir open(%s) failed: %s", path, strerror(err));
    }
    int rc = fsync(fd);
    int err = errno;
    close(fd);
    free(copy);
    if (rc != 0) {
        return luaL_error(L, "umbel.sys.sync_dir fsync failed: %s", strerror(err));
    }
    lua_pushboolean(L, 1);
    return 1;
}

static int l_flock(lua_State *L) {
    FILE *f = check_file(L, 1);
    const char *mode = luaL_checkstring(L, 2);
    int op = 0;

    if (strcmp(mode, "sh") == 0) {
        op = LOCK_SH;
    } else if (strcmp(mode, "ex") == 0) {
        op = LOCK_EX;
    } else if (strcmp(mode, "un") == 0) {
        op = LOCK_UN;
    } else if (strcmp(mode, "sh_nb") == 0) {
        op = LOCK_SH | LOCK_NB;
    } else if (strcmp(mode, "ex_nb") == 0) {
        op = LOCK_EX | LOCK_NB;
    } else {
        return luaL_error(L, "umbel.sys.flock: unknown mode '%s'", mode);
    }

    if (flock(fileno(f), op) != 0) {
        if (errno == EWOULDBLOCK) {
            lua_pushboolean(L, 0);
            lua_pushliteral(L, "wouldblock");
            return 2;
        }
        return luaL_error(L, "umbel.sys.flock failed: %s", strerror(errno));
    }
    lua_pushboolean(L, 1);
    return 1;
}

static int l_pid(lua_State *L) {
    lua_pushinteger(L, (lua_Integer)getpid());
    return 1;
}

static const luaL_Reg sys_funcs[] = {
    {"fsync", l_fsync},
    {"fdatasync", l_fdatasync},
    {"sync-dir", l_sync_dir},
    {"flock", l_flock},
    {"pid", l_pid},
    {NULL, NULL},
};

int luaopen_umbel_sys(lua_State *L) {
    luaL_newlib(L, sys_funcs);
    return 1;
}
