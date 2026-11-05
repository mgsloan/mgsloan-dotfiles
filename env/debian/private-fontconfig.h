#include <fontconfig/fontconfig.h>
#include <pango/pangocairo.h>
#include <pango/pangofc-fontmap.h>

static void setup_private_fontconfig(void) {
    FcConfig *config = FcConfigCreate();
    if (!config ||
        !FcConfigParseAndLoad(config, (const FcChar8 *)"/usr/share/env-desktop/fonts.conf", FcTrue) ||
        !FcConfigBuildFonts(config)) {
        g_error("Unable to load private fontconfig");
    }
    PangoFontMap *map = pango_cairo_font_map_new();
    pango_fc_font_map_set_config(PANGO_FC_FONT_MAP(map), config);
    pango_cairo_font_map_set_default(PANGO_CAIRO_FONT_MAP(map));
    g_object_unref(map);
    FcConfigDestroy(config);
}
