// this is from OneSadCookie's WebGLGame (which is from .... ->)


// this file copied, refactored, etc. from utils3d.js

// The WebGL spec was recently updated to replace the Canvas prefix on types with the WebGL prefix.
// For compatibility reasons we set up aliases to from the WebGL prefixed typename to the
// Canvas prefixed name for the benefit of older builds of WebKit and Mozilla
if (!("WebGLFloatArray" in window))
    WebGLFloatArray = window.CanvasFloatArray;
if (!("WebGLByteArray" in window))
    WebGLByteArray = window.CanvasByteArray;
if (!("WebGLIntArray" in window))
    WebGLIntArray = window.CanvasIntArray;
if (!("WebGLShortArray" in window))
    WebGLShortArray = window.CanvasShortArray;
if (!("WebGLUnsignedByteArray" in window))
    WebGLUnsignedByteArray = window.CanvasUnsignedByteArray;
if (!("WebGLUnsignedIntArray" in window))
    WebGLUnsignedIntArray = window.CanvasUnsignedIntArray;
if (!("WebGLUnsignedShortArray" in window))
    WebGLUnsignedShortArray = window.CanvasUnsignedShortArray;

WEBGL_IDENTIFIERS = [
    'experimental-webgl', // current standard
    'webkit-3d',          // older webkits
    'moz-webgl'           // older geckos
];

function initWebGL(canvasName)
{
    var canvas = document.getElementById(canvasName)
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
    
    for (var i=0 ; i<3 ; i++) //WEBGL_IDENTIFIERS.size(); ++i)
    {
        try
        {
            var gl = canvas.getContext(WEBGL_IDENTIFIERS[i])
            if (gl)
            {
                if (!gl.getShaderParameter)
                {
                    // work around API change
                    // this may not be correct for all cases
                    gl.getShaderParameter = gl.getShaderi
                }
                if (!gl.getProgramParameter)
                {
                    // work around API change
                    // this may not be correct for all cases
                    gl.getProgramParameter = gl.getProgrami
                }
                return gl
            }
        }
        catch(e) {}
    }
        
    alert("No WebGL context found")
    return null
}

function compileShader(url, gl, shaderType, resman, closure)
{
    xhrText(url, resman, function(text)
    {
        var shader = gl.createShader(shaderType)
        gl.shaderSource(shader, text)

        gl.compileShader(shader)

        var compiled = gl.getShaderParameter(shader, gl.COMPILE_STATUS)
        if (compiled)
        {
            closure(shader)
            resman.loaded('shader compile of ' + url)
        }
        else
        {
            var error = gl.getShaderInfoLog(shader)
            console.log("*** Error compiling shader '" + url + "':" + error)
            console.log("shader text:\n" + text + "\n")
            gl.deleteShader(shader)
            resman.error('shader compile of ' + url)
        }
    })
}

function st(gl, shaderType)
{
    if (shaderType == gl.VERTEX_SHADER) return 'vertex'
    if (shaderType == gl.FRAGMENT_SHADER) return 'fragment'
}

function _linkProgram(gl, status, shaderType, shader, resman, closure)
{
    status[shaderType] = shader
    if (status[gl.VERTEX_SHADER] && status[gl.FRAGMENT_SHADER])
    {
        var program = gl.createProgram()

        gl.attachShader(program, status[gl.VERTEX_SHADER])
        gl.attachShader(program, status[gl.FRAGMENT_SHADER])

        gl.linkProgram(program)

        var linked = gl.getProgramParameter(program, gl.LINK_STATUS)
        if (linked)
        {
            closure(program)
            resman.loaded('program link')
        }
        else
        {
            var error = gl.getProgramInfoLog(program)
            console.log("Error linking program: " + error)

            gl.deleteProgram(program)
            gl.deleteShader(status[gl.VERTEX_SHADER])
            gl.deleteShader(status[gl.FRAGMENT_SHADER])

            resman.error('program link')
        }
    }
}

function linkProgram(vsurl, fsurl, gl, resman, closure)
{
    var status = {}
    
    compileShader(vsurl, gl, gl.VERTEX_SHADER, resman, function(shader)
    {
        _linkProgram(gl, status, gl.VERTEX_SHADER, shader, resman, closure)
    })
    compileShader(fsurl, gl, gl.FRAGMENT_SHADER, resman, function(shader)
    {
        _linkProgram(gl, status, gl.FRAGMENT_SHADER, shader, resman, closure)
    })
}

function xhrImage(url, resman, closure)
{
    var image = new Image()
    image.observe('load', function(event)
    {
        closure(image)
        resman.loaded('Image ' + url)
    })
    image.observe('error', function(event)
    {
        resman.error('Image ' + url)
    })
    image.observe('abort', function(event)
    {
        resman.error('Image ' + url)
    })
    image.src = url
}

function loadTexture(url, gl, resman, closure)
{
    xhrImage(url, resman, function(image)
    {
        // Due to a bug in current WebKit WebGL, transparent areas of images
        // are loaded with junk in them.  This requires a patch to the
        // imageToTexture() function in
        //     WebCore/platform/graphics/mac/GraphicsContext3DMac.cpp
        // (adding a memset, or CGClearRect, or similar to zero the bitmap
        // context's data before drawing the image).
        // Alternatively, could potentially load an uncompressed image from
        // a WebGLArray object, rather than loading a "real" image format
        // from an HTMLImageElement object.
        
        var texture = gl.createTexture()
        gl.enable(gl.TEXTURE_2D)
        gl.bindTexture(gl.TEXTURE_2D, texture)
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR)
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)
        gl.texImage2D(gl.TEXTURE_2D, 0, image)
        
        closure(image, texture)
        resman.loaded('Texture of ' + url)
    })
}
