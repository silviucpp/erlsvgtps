
-define(ALLOWED_ELEMENTS_NAME, [
    <<"circle">>, <<"defs">>, <<"desc">>, <<"ellipse">>, <<"g">>, <<"line">>, <<"linearGradient">>, <<"path">>,
    <<"polygon">>, <<"polyline">>, <<"radialGradient">>, <<"rect">>, <<"solidColor">>, <<"svg">>, <<"text">>,
    <<"textArea">>, <<"title">>, <<"use">>
]).

-define(SVG_ALLOWED_ATTRIBUTES, [
    <<"about">>, <<"baseProfile">>, <<"class">>, <<"color">>, <<"color-rendering">>, <<"content">>, <<"contentScriptType">>, <<"datatype">>,
    <<"direction">>, <<"display-align">>, <<"externalResourcesRequired">>, <<"fill">>, <<"fill-opacity">>, <<"fill-rule">>, <<"focusable">>,
    <<"font-family">>, <<"font-size">>, <<"font-style">>, <<"font-variant">>, <<"font-weight">>, <<"height">>, <<"line-increment">>,
    <<"playbackOrder">>, <<"preserveAspectRatio">>, <<"property">>, <<"rel">>, <<"resource">>, <<"rev">>, <<"role">>, <<"snapshotTime">>,
    <<"solid-color">>, <<"solid-opacity">>, <<"stop-color">>, <<"stop-opacity">>, <<"stroke">>, <<"stroke-dasharray">>, <<"stroke-dashoffset">>,
    <<"stroke-linecap">>, <<"stroke-linejoin">>, <<"stroke-miterlimit">>, <<"stroke-opacity">>, <<"stroke-width">>, <<"text-align">>,
    <<"text-anchor">>, <<"timelineBegin">>, <<"typeof">>, <<"unicode-bidi">>, <<"vector-effect">>, <<"version">>, <<"viewBox">>, <<"width">>,
    <<"xml:base">>, <<"xml:lang">>, <<"xml:space">>, <<"zoomAndPan">>
]).

-define(DESC_ALLOWED_ATTRIBUTES, [
    <<"about">>, <<"buffered-rendering">>, <<"class">>, <<"content">>, <<"datatype">>, <<"display">>, <<"id">>, <<"image-rendering">>,
    <<"property">>, <<"rel">>, <<"requiredFonts">>, <<"resource">>, <<"rev">>, <<"role">>, <<"shape-rendering">>, <<"systemLanguage">>,
    <<"text-rendering">>, <<"typeof">>, <<"viewport-fill">>, <<"viewport-fill-opacity">>, <<"visibility">>, <<"xml:base">>, <<"xml:id">>,
    <<"xml:lang">>, <<"xml:space">>
]).

-define(TITLE_ALLOWED_ATTRIBUTES, [
    <<"about">>, <<"buffered-rendering">>, <<"class">>, <<"content">>, <<"datatype">>, <<"display">>, <<"id">>, <<"image-rendering">>,
    <<"property">>, <<"rel">>, <<"requiredFonts">>, <<"resource">>, <<"rev">>, <<"role">>, <<"shape-rendering">>, <<"systemLanguage">>,
    <<"text-rendering">>, <<"typeof">>, <<"viewport-fill">>, <<"viewport-fill-opacity">>, <<"visibility">>, <<"xml:base">>, <<"xml:id">>,
    <<"xml:lang">>, <<"xml:space">>
]).

-define(PATH_ALLOWED_ATTRIBUTES, [
    <<"about">>, <<"class">>, <<"color">>, <<"color-rendering">>, <<"content">>, <<"d">>, <<"datatype">>, <<"direction">>, <<"display-align">>,
    <<"fill">>, <<"fill-opacity">>, <<"fill-rule">>, <<"font-family">>, <<"font-size">>, <<"font-style">>, <<"font-variant">>, <<"font-weight">>,
    <<"id">>, <<"line-increment">>, <<"pathLength">>, <<"property">>, <<"rel">>, <<"requiredFonts">>, <<"resource">>, <<"rev">>, <<"role">>,
    <<"solid-color">>, <<"solid-opacity">>, <<"stop-color">>, <<"stop-opacity">>, <<"stroke">>, <<"stroke-dasharray">>, <<"stroke-dashoffset">>,
    <<"stroke-linecap">>, <<"stroke-linejoin">>, <<"stroke-miterlimit">>, <<"stroke-opacity">>, <<"stroke-width">>, <<"systemLanguage">>,
    <<"text-align">>, <<"text-anchor">>, <<"transform">>, <<"typeof">>, <<"unicode-bidi">>, <<"vector-effect">>, <<"xml:base">>, <<"xml:id">>,
    <<"xml:lang">>, <<"xml:space">>
]).

-define(RECT_ALLOWED_ATTRIBUTES, [
    <<"about">>, <<"class">>, <<"color">>, <<"color-rendering">>, <<"content">>, <<"datatype">>, <<"direction">>, <<"display-align">>,
    <<"fill">>, <<"fill-opacity">>, <<"fill-rule">>, <<"font-family">>, <<"font-size">>, <<"font-style">>, <<"font-variant">>, <<"font-weight">>,
    <<"height">>, <<"id">>, <<"line-increment">>, <<"property">>, <<"rel">>, <<"requiredFonts">>, <<"resource">>, <<"rev">>, <<"role">>,
    <<"rx">>, <<"ry">>, <<"solid-color">>, <<"solid-opacity">>, <<"stop-color">>, <<"stop-opacity">>, <<"stroke">>, <<"stroke-dasharray">>,
    <<"stroke-dashoffset">>, <<"stroke-linecap">>, <<"stroke-linejoin">>, <<"stroke-miterlimit">>, <<"stroke-opacity">>, <<"stroke-width">>,
    <<"systemLanguage">>, <<"text-align">>, <<"text-anchor">>, <<"transform">>, <<"typeof">>, <<"unicode-bidi">>, <<"vector-effect">>,
    <<"width">>, <<"x">>, <<"xml:base">>, <<"xml:id">>, <<"xml:lang">>, <<"xml:space">>, <<"y">>
]).

-define(CIRCLE_ALLOWED_ATTRIBUTES, [
    <<"about">>, <<"class">>, <<"color">>, <<"color-rendering">>, <<"content">>,
    <<"cx">>, <<"cy">>, <<"datatype">>, <<"direction">>, <<"display-align">>,
    <<"fill">>, <<"fill-opacity">>, <<"fill-rule">>, <<"font-family">>, <<"font-size">>,
    <<"font-style">>, <<"font-variant">>, <<"font-weight">>, <<"id">>, <<"line-increment">>,
    <<"property">>, <<"r">>, <<"rel">>, <<"requiredFonts">>, <<"resource">>,
    <<"rev">>, <<"role">>, <<"solid-color">>, <<"solid-opacity">>, <<"stop-color">>,
    <<"stop-opacity">>, <<"stroke">>, <<"stroke-dasharray">>, <<"stroke-dashoffset">>,
    <<"stroke-linecap">>, <<"stroke-linejoin">>, <<"stroke-miterlimit">>, <<"stroke-opacity">>,
    <<"stroke-width">>, <<"systemLanguage">>, <<"text-align">>, <<"text-anchor">>,
    <<"transform">>, <<"typeof">>, <<"unicode-bidi">>, <<"vector-effect">>, <<"xml:base">>,
    <<"xml:id">>, <<"xml:lang">>, <<"xml:space">>
]).

-define(LINE_ALLOWED_ATTRIBUTES, [
    <<"about">>, <<"class">>, <<"color">>, <<"color-rendering">>, <<"content">>,
    <<"datatype">>, <<"direction">>, <<"display-align">>, <<"fill">>, <<"fill-opacity">>,
    <<"fill-rule">>, <<"font-family">>, <<"font-size">>, <<"font-style">>, <<"font-variant">>,
    <<"font-weight">>, <<"id">>, <<"line-increment">>, <<"property">>, <<"rel">>,
    <<"requiredFonts">>, <<"resource">>, <<"rev">>, <<"role">>, <<"solid-color">>,
    <<"solid-opacity">>, <<"stop-color">>, <<"stop-opacity">>, <<"stroke">>, <<"stroke-dasharray">>,
    <<"stroke-dashoffset">>, <<"stroke-linecap">>, <<"stroke-linejoin">>, <<"stroke-miterlimit">>,
    <<"stroke-opacity">>, <<"stroke-width">>, <<"systemLanguage">>, <<"text-align">>, <<"text-anchor">>,
    <<"transform">>, <<"typeof">>, <<"unicode-bidi">>, <<"vector-effect">>, <<"x1">>, <<"x2">>,
    <<"xml:base">>, <<"xml:id">>, <<"xml:lang">>, <<"xml:space">>, <<"y1">>, <<"y2">>
]).

-define(ELLIPSE_ALLOWED_ATTRIBUTES, [
    <<"about">>, <<"class">>, <<"color">>, <<"color-rendering">>, <<"content">>, <<"cx">>,
    <<"cy">>, <<"datatype">>, <<"direction">>, <<"display-align">>, <<"fill">>, <<"fill-opacity">>,
    <<"fill-rule">>, <<"font-family">>, <<"font-size">>, <<"font-style">>, <<"font-variant">>,
    <<"font-weight">>, <<"id">>, <<"line-increment">>, <<"property">>, <<"rel">>, <<"requiredFonts">>,
    <<"resource">>, <<"rev">>, <<"role">>, <<"rx">>, <<"ry">>, <<"solid-color">>, <<"solid-opacity">>,
    <<"stop-color">>, <<"stop-opacity">>, <<"stroke">>, <<"stroke-dasharray">>, <<"stroke-dashoffset">>,
    <<"stroke-linecap">>, <<"stroke-linejoin">>, <<"stroke-miterlimit">>, <<"stroke-opacity">>,
    <<"stroke-width">>, <<"systemLanguage">>, <<"text-align">>, <<"text-anchor">>, <<"transform">>,
    <<"typeof">>, <<"unicode-bidi">>, <<"vector-effect">>, <<"xml:base">>, <<"xml:id">>, <<"xml:lang">>,
    <<"xml:space">>
]).

-define(POLYLINE_ALLOWED_ATTRIBUTES, [
    <<"about">>, <<"class">>, <<"color">>, <<"color-rendering">>, <<"content">>, <<"datatype">>,
    <<"direction">>, <<"display-align">>, <<"fill">>, <<"fill-opacity">>, <<"fill-rule">>, <<"font-family">>,
    <<"font-size">>, <<"font-style">>, <<"font-variant">>, <<"font-weight">>, <<"id">>, <<"line-increment">>,
    <<"points">>, <<"property">>, <<"rel">>, <<"requiredFonts">>, <<"resource">>, <<"rev">>, <<"role">>,
    <<"solid-color">>, <<"solid-opacity">>, <<"stop-color">>, <<"stop-opacity">>, <<"stroke">>,
    <<"stroke-dasharray">>, <<"stroke-dashoffset">>, <<"stroke-linecap">>, <<"stroke-linejoin">>,
    <<"stroke-miterlimit">>, <<"stroke-opacity">>, <<"stroke-width">>, <<"systemLanguage">>,
    <<"text-align">>, <<"text-anchor">>, <<"transform">>, <<"typeof">>, <<"unicode-bidi">>,
    <<"vector-effect">>, <<"xml:base">>, <<"xml:id">>, <<"xml:lang">>, <<"xml:space">>
]).

-define(POLYGON_ALLOWED_ATTRIBUTES, [
    <<"about">>, <<"class">>, <<"color">>, <<"color-rendering">>, <<"content">>, <<"datatype">>, <<"direction">>,
    <<"display-align">>, <<"fill">>, <<"fill-opacity">>, <<"fill-rule">>, <<"font-family">>, <<"font-size">>,
    <<"font-style">>, <<"font-variant">>, <<"font-weight">>, <<"id">>, <<"line-increment">>, <<"points">>,
    <<"property">>, <<"rel">>, <<"requiredFonts">>, <<"resource">>, <<"rev">>, <<"role">>, <<"solid-color">>,
    <<"solid-opacity">>, <<"stop-color">>, <<"stop-opacity">>, <<"stroke">>, <<"stroke-dasharray">>, <<"stroke-dashoffset">>,
    <<"stroke-linecap">>, <<"stroke-linejoin">>, <<"stroke-miterlimit">>, <<"stroke-opacity">>, <<"stroke-width">>,
    <<"systemLanguage">>, <<"text-align">>, <<"text-anchor">>, <<"transform">>, <<"typeof">>, <<"unicode-bidi">>,
    <<"vector-effect">>, <<"xml:base">>, <<"xml:id">>, <<"xml:lang">>, <<"xml:space">>
]).

-define(SOLIDCOLOR_ALLOWED_ATTRIBUTES, [
    <<"about">>, <<"class">>, <<"color">>, <<"color-rendering">>, <<"content">>, <<"datatype">>, <<"direction">>,
    <<"display-align">>, <<"fill">>, <<"fill-opacity">>, <<"fill-rule">>, <<"font-family">>, <<"font-size">>,
    <<"font-style">>, <<"font-variant">>, <<"font-weight">>, <<"id">>, <<"line-increment">>, <<"property">>,
    <<"rel">>, <<"resource">>, <<"rev">>, <<"role">>, <<"solid-color">>, <<"solid-opacity">>, <<"stop-color">>,
    <<"stop-opacity">>, <<"stroke">>, <<"stroke-dasharray">>, <<"stroke-dashoffset">>, <<"stroke-linecap">>,
    <<"stroke-linejoin">>, <<"stroke-miterlimit">>, <<"stroke-opacity">>, <<"stroke-width">>, <<"text-align">>,
    <<"text-anchor">>, <<"typeof">>, <<"unicode-bidi">>, <<"vector-effect">>, <<"xml:base">>, <<"xml:id">>,
    <<"xml:lang">>, <<"xml:space">>
]).

-define(TEXTAREA_ALLOWED_ATTRIBUTES, [
    <<"about">>, <<"class">>, <<"color">>, <<"color-rendering">>, <<"content">>, <<"datatype">>, <<"direction">>,
    <<"display-align">>, <<"fill">>, <<"fill-opacity">>, <<"fill-rule">>, <<"font-family">>, <<"font-size">>,
    <<"font-style">>, <<"font-variant">>, <<"font-weight">>, <<"height">>, <<"id">>, <<"line-increment">>,
    <<"property">>, <<"rel">>, <<"requiredFonts">>, <<"resource">>, <<"rev">>, <<"role">>, <<"solid-color">>,
    <<"solid-opacity">>, <<"stop-color">>, <<"stop-opacity">>, <<"stroke">>, <<"stroke-dasharray">>, <<"stroke-dashoffset">>,
    <<"stroke-linecap">>, <<"stroke-linejoin">>, <<"stroke-miterlimit">>, <<"stroke-opacity">>, <<"stroke-width">>,
    <<"systemLanguage">>, <<"text-align">>, <<"text-anchor">>, <<"transform">>, <<"typeof">>, <<"unicode-bidi">>,
    <<"vector-effect">>, <<"width">>, <<"x">>, <<"xml:base">>, <<"xml:id">>, <<"xml:lang">>, <<"xml:space">>, <<"y">>
]).

-define(LINEARGRADIENT_ALLOWED_ATTRIBUTES, [
    <<"about">>, <<"class">>, <<"color">>, <<"color-rendering">>, <<"content">>, <<"datatype">>, <<"direction">>,
    <<"display-align">>, <<"fill">>, <<"fill-opacity">>, <<"fill-rule">>, <<"font-family">>, <<"font-size">>,
    <<"font-style">>, <<"font-variant">>, <<"font-weight">>, <<"gradientUnits">>, <<"id">>, <<"line-increment">>,
    <<"property">>, <<"rel">>, <<"resource">>, <<"rev">>, <<"role">>, <<"solid-color">>, <<"solid-opacity">>,
    <<"stop-color">>, <<"stop-opacity">>, <<"stroke">>, <<"stroke-dasharray">>, <<"stroke-dashoffset">>,
    <<"stroke-linecap">>, <<"stroke-linejoin">>, <<"stroke-miterlimit">>, <<"stroke-opacity">>, <<"stroke-width">>,
    <<"text-align">>, <<"text-anchor">>, <<"typeof">>, <<"unicode-bidi">>, <<"vector-effect">>, <<"x1">>, <<"x2">>,
    <<"xml:base">>, <<"xml:id">>, <<"xml:lang">>, <<"xml:space">>, <<"y1">>, <<"y2">>
]).

-define(RADIALGRADIENT_ALLOWED_ATTRIBUTES, [
    <<"about">>, <<"class">>, <<"color">>, <<"color-rendering">>, <<"content">>, <<"cx">>, <<"cy">>, <<"datatype">>,
    <<"direction">>, <<"display-align">>, <<"fill">>, <<"fill-opacity">>, <<"fill-rule">>, <<"font-family">>,
    <<"font-size">>, <<"font-style">>, <<"font-variant">>, <<"font-weight">>, <<"gradientUnits">>, <<"id">>,
    <<"line-increment">>, <<"property">>, <<"r">>, <<"rel">>, <<"resource">>, <<"rev">>, <<"role">>, <<"solid-color">>,
    <<"solid-opacity">>, <<"stop-color">>, <<"stop-opacity">>, <<"stroke">>, <<"stroke-dasharray">>, <<"stroke-dashoffset">>,
    <<"stroke-linecap">>, <<"stroke-linejoin">>, <<"stroke-miterlimit">>, <<"stroke-opacity">>, <<"stroke-width">>,
    <<"text-align">>, <<"text-anchor">>, <<"typeof">>, <<"unicode-bidi">>, <<"vector-effect">>, <<"xml:base">>,
    <<"xml:id">>, <<"xml:lang">>, <<"xml:space">>
]).

-define(TEXT_ALLOWED_ATTRIBUTES, [
    <<"about">>, <<"class">>, <<"color">>, <<"color-rendering">>, <<"content">>, <<"datatype">>, <<"direction">>,
    <<"display-align">>, <<"editable">>, <<"fill">>, <<"fill-opacity">>, <<"fill-rule">>, <<"font-family">>,
    <<"font-size">>, <<"font-style">>, <<"font-variant">>, <<"font-weight">>, <<"id">>, <<"line-increment">>,
    <<"property">>, <<"rel">>, <<"requiredFonts">>, <<"resource">>, <<"rev">>, <<"role">>, <<"rotate">>, <<"solid-color">>,
    <<"solid-opacity">>, <<"stop-color">>, <<"stop-opacity">>, <<"stroke">>, <<"stroke-dasharray">>, <<"stroke-dashoffset">>,
    <<"stroke-linecap">>, <<"stroke-linejoin">>, <<"stroke-miterlimit">>, <<"stroke-opacity">>, <<"stroke-width">>,
    <<"systemLanguage">>, <<"text-align">>, <<"text-anchor">>, <<"transform">>, <<"typeof">>, <<"unicode-bidi">>,
    <<"vector-effect">>, <<"x">>, <<"xml:base">>, <<"xml:id">>, <<"xml:lang">>, <<"xml:space">>, <<"y">>
]).

-define(G_ALLOWED_ATTRIBUTES, [
    <<"about">>, <<"class">>, <<"color">>, <<"color-rendering">>, <<"content">>, <<"datatype">>, <<"direction">>,
    <<"display-align">>, <<"fill">>, <<"fill-opacity">>, <<"fill-rule">>, <<"font-family">>, <<"font-size">>,
    <<"font-style">>, <<"font-variant">>, <<"font-weight">>, <<"id">>, <<"line-increment">>, <<"property">>,
    <<"rel">>, <<"requiredFonts">>, <<"resource">>, <<"rev">>, <<"role">>, <<"solid-color">>, <<"solid-opacity">>,
    <<"stop-color">>, <<"stop-opacity">>, <<"stroke">>, <<"stroke-dasharray">>, <<"stroke-dashoffset">>,
    <<"stroke-linecap">>, <<"stroke-linejoin">>, <<"stroke-miterlimit">>, <<"stroke-opacity">>, <<"stroke-width">>,
    <<"systemLanguage">>, <<"text-align">>, <<"text-anchor">>, <<"transform">>, <<"typeof">>, <<"unicode-bidi">>,
    <<"vector-effect">>, <<"xml:base">>, <<"xml:id">>, <<"xml:lang">>, <<"xml:space">>
]).

-define(DEFS_ALLOWED_ATTRIBUTES, [
    <<"about">>, <<"class">>, <<"color">>, <<"color-rendering">>, <<"content">>, <<"datatype">>, <<"direction">>,
    <<"display-align">>, <<"fill">>, <<"fill-opacity">>, <<"fill-rule">>, <<"font-family">>, <<"font-size">>,
    <<"font-style">>, <<"font-variant">>, <<"font-weight">>, <<"id">>, <<"line-increment">>, <<"property">>,
    <<"rel">>, <<"resource">>, <<"rev">>, <<"role">>, <<"solid-color">>, <<"solid-opacity">>, <<"stop-color">>,
    <<"stop-opacity">>, <<"stroke">>, <<"stroke-dasharray">>, <<"stroke-dashoffset">>, <<"stroke-linecap">>,
    <<"stroke-linejoin">>, <<"stroke-miterlimit">>, <<"stroke-opacity">>, <<"stroke-width">>, <<"text-align">>,
    <<"text-anchor">>, <<"typeof">>, <<"unicode-bidi">>, <<"vector-effect">>, <<"xml:base">>, <<"xml:id">>,
    <<"xml:lang">>, <<"xml:space">>
]).

-define(USE_ALLOWED_ATTRIBUTES, [
    <<"about">>, <<"class">>, <<"color">>, <<"color-rendering">>, <<"content">>, <<"datatype">>, <<"direction">>,
    <<"display-align">>, <<"fill">>, <<"fill-opacity">>, <<"fill-rule">>, <<"font-family">>, <<"font-size">>,
    <<"font-style">>, <<"font-variant">>, <<"font-weight">>, <<"href">>, <<"id">>, <<"line-increment">>,
    <<"property">>, <<"rel">>, <<"requiredFonts">>, <<"resource">>, <<"rev">>, <<"role">>, <<"solid-color">>,
    <<"solid-opacity">>, <<"stop-color">>, <<"stop-opacity">>, <<"stroke">>, <<"stroke-dasharray">>,
    <<"stroke-dashoffset">>, <<"stroke-linecap">>, <<"stroke-linejoin">>, <<"stroke-miterlimit">>,
    <<"stroke-opacity">>, <<"stroke-width">>, <<"systemLanguage">>, <<"text-align">>, <<"text-anchor">>,
    <<"transform">>, <<"typeof">>, <<"unicode-bidi">>, <<"vector-effect">>, <<"x">>, <<"xml:base">>,
    <<"xml:id">>, <<"xml:lang">>, <<"xml:space">>, <<"y">>
]).

-define(ALLOWED_ATTRIBUTES, #{
    <<"circle">> => ?CIRCLE_ALLOWED_ATTRIBUTES,
    <<"defs">> => ?DEFS_ALLOWED_ATTRIBUTES,
    <<"desc">> => ?DESC_ALLOWED_ATTRIBUTES,
    <<"ellipse">> => ?ELLIPSE_ALLOWED_ATTRIBUTES,
    <<"g">> => ?G_ALLOWED_ATTRIBUTES,
    <<"line">> => ?LINE_ALLOWED_ATTRIBUTES,
    <<"linearGradient">> => ?LINEARGRADIENT_ALLOWED_ATTRIBUTES,
    <<"path">> => ?PATH_ALLOWED_ATTRIBUTES,
    <<"polygon">> => ?POLYGON_ALLOWED_ATTRIBUTES,
    <<"polyline">> => ?POLYLINE_ALLOWED_ATTRIBUTES,
    <<"radialGradient">> => ?RADIALGRADIENT_ALLOWED_ATTRIBUTES,
    <<"rect">> => ?RECT_ALLOWED_ATTRIBUTES,
    <<"solidColor">> => ?SOLIDCOLOR_ALLOWED_ATTRIBUTES,
    <<"svg">> => ?SVG_ALLOWED_ATTRIBUTES,
    <<"text">> => ?TEXT_ALLOWED_ATTRIBUTES,
    <<"textArea">> => ?TEXTAREA_ALLOWED_ATTRIBUTES,
    <<"title">> => ?TITLE_ALLOWED_ATTRIBUTES,
    <<"use">> => ?USE_ALLOWED_ATTRIBUTES
}).
