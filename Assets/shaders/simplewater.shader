FEATURES
{
    #include "common/features.hlsl"

    //
    // Expensive runtime features
    //
    Feature( F_DYNAMIC_REFLECTIONS, 0..2 ( 0="Disabled", 1="Aliased", 2="Anti-aliased" ), "Water" );
    Feature( F_CAUSTICS, 0..2 ( 0="Disabled", 1="Texture", 2="Flipbook" ), "Water" );
    Feature( F_INDIRECT, 0..3 ( 0="Disabled", 1="Low Quality", 2="High Quality", 3="Stupid Quality (Do Not Use)" ), "Water" );
    Feature( F_PROCEDURAL, 0..1, "Water" );
}

MODES
{
	Forward();
    Depth();
}   

COMMON
{
	#include "common/shared.hlsl"
}

struct VertexInput
{
	#include "common/vertexinput.hlsl"
    float4 vColor : COLOR0 < Semantic( Color ); >;
};

struct PixelInput
{
	#include "common/pixelinput.hlsl"
};

VS
{
    #include "common/vertex.hlsl"

    PixelInput MainVs( VertexInput i )
    {
        PixelInput o = ProcessVertex( i );
        o.vVertexColor.rgb = i.vColor.rgb;
		o = FinalizeVertex( o );
        return o;
    }
}

//=========================================================================================================================

PS
{
	StaticCombo( S_DYNAMIC_REFLECTIONS, F_DYNAMIC_REFLECTIONS, Sys( ALL ) );
    StaticCombo( S_CAUSTICS, F_CAUSTICS, Sys( ALL ) );
    StaticCombo( S_INDIRECT, F_INDIRECT, Sys( ALL ) );
    StaticCombo( S_PROCEDURAL, F_PROCEDURAL, Sys( ALL ) );

    //
    // Comment out the above and uncomment these if you want shorter compile times (for iteration)
    //    
    // #define S_PROCEDURAL 1
    // #define S_INDIRECT 2
    // #define S_CAUSTICS 2
    // #define S_DYNAMIC_REFLECTIONS 2

    #include "sbox_pixel.fxc"
    #include "procedural.hlsl"
    #include "common/utils/normal.hlsl"

    SamplerState g_sSampler < Filter( ANISO ); AddressU( WRAP ); AddressV( WRAP ); >;
    SamplerState g_sClampSampler < Filter( ANISO ); AddressU( CLAMP ); AddressV( CLAMP ); >;

    //
    // Inexpensive runtime features
    //
    bool g_bSheen < Default( 1 ); UiGroup( "Features,-1000/10" ); >;
    bool g_bWaterline < Default( 1 ); UiGroup( "Features,-1000/10" ); >;
    bool g_bRefractionDepthTest < Default( 1 ); UiGroup( "Features,-1000/10" ); >;

	//
	// Material
	//
    float g_flMetallic < UiType( Slider ); Range( 0, 1 ); Default( 0.0 ); UiGroup( "Material,10/30" ); >;
    float g_flOpacity < UiType( Slider ); Range( 0, 1 ); Default( 1.0 ); UiGroup( "Material,10/30" ); >;

	//
	// Waves
	//
    float g_flFeatherDistance < UiType( Slider ); Range( 0, 512 ); Default( 1.0 ); UiGroup( "Waves,10/30" ); >;
	float g_flTilingScale < UiType( Slider ); Range( 0, 12 ); Default( 9 ); UiGroup( "Waves,10/30" ); >;
    float g_flNormalScale < UiType( Slider ); Range( 0, 1 ); Default( 1.0 ); UiGroup( "Waves,10/30" ); >;
    float g_flRefractionScale < UiType( Slider ); Range( 0, 1 ); Default( 1.0 ); UiGroup( "Waves,10/30" ); >;

    CreateInputTexture2D( NormalMapA, Linear, 8, "", "_normal", "Waves,10/10", Default3( 1.0, 1.0, 1.0 ) );
    Texture2D g_tNormalMapA < Channel( RGB, Box( NormalMapA ), Linear ); OutputFormat( BC7 ); SrgbRead( false ); >;

    CreateInputTexture2D( NormalMapB, Linear, 8, "", "_normal", "Waves,10/10", Default3( 1.0, 1.0, 1.0 ) );
    Texture2D g_tNormalMapB < Channel( RGB, Box( NormalMapB ), Linear ); OutputFormat( BC7 ); SrgbRead( false ); >;

    CreateInputTexture2D( NormalMapC, Linear, 8, "", "_normal", "Waves,10/10", Default3( 1.0, 1.0, 1.0 ) );
    Texture2D g_tNormalMapC < Channel( RGB, Box( NormalMapC ), Linear ); OutputFormat( BC7 ); SrgbRead( false ); >;

    CreateInputTexture2D( NormalMapD, Linear, 8, "", "_normal", "Waves,10/10", Default3( 1.0, 1.0, 1.0 ) );
    Texture2D g_tNormalMapD < Channel( RGB, Box( NormalMapD ), Linear ); OutputFormat( BC7 ); SrgbRead( false ); >;

    float g_flScale < UiType( Slider ); Range( 1, 10 ); UiGroup( "Waves,10/30" ); >;
    int g_nFrequencyCount < Default( 4 ); Range( 1, 4 ); UiGroup( "Waves,10/30" ); >;

    int g_nBlurLevel < Default( 0 ); UiGroup( "Waves,10/30" ); Range( 0, 4 ); >;

    float2 g_flSpeedA < UiType( Slider ); Range2( -1, -1, 1, 1 ); UiGroup( "Waves,10/A,30" ); >;
    float2 g_flSpeedB < UiType( Slider ); Range2( -1, -1, 1, 1 ); UiGroup( "Waves,10/B,30" ); >;
    float2 g_flSpeedC < UiType( Slider ); Range2( -1, -1, 1, 1 ); UiGroup( "Waves,10/C,30" ); >;
    float2 g_flSpeedD < UiType( Slider ); Range2( -1, -1, 1, 1 ); UiGroup( "Waves,10/D,30" ); >;

    float2 g_flScaleA < UiType( Slider ); Range2( -1, -1, 4, 4 ); UiGroup( "Waves,10/A,30" ); >;
    float2 g_flScaleB < UiType( Slider ); Range2( -1, -1, 4, 4 ); UiGroup( "Waves,10/B,30" ); >;
    float2 g_flScaleC < UiType( Slider ); Range2( -1, -1, 4, 4 ); UiGroup( "Waves,10/C,30" ); >;
    float2 g_flScaleD < UiType( Slider ); Range2( -1, -1, 4, 4 ); UiGroup( "Waves,10/D,30" ); >;

    float g_flFrequencyA < Default( 1.0 ); Range( 0, 100 ); UiGroup( "Waves,10/A,30" ); >;
    float g_flFrequencyB < Default( 5.0 ); Range( 0, 100 ); UiGroup( "Waves,10/B,30" ); >;
    float g_flFrequencyC < Default( 10.0 ); Range( 0, 100 ); UiGroup( "Waves,10/C,30" ); >;
    float g_flFrequencyD < Default( 20.0 ); Range( 0, 100 ); UiGroup( "Waves,10/D,30" ); >;

    float g_flStrengthA < Default( 0.1 ); Range( 0, 1 ); UiGroup( "Waves,10/A,30" ); >;
    float g_flStrengthB < Default( 0.05 ); Range( 0, 1 ); UiGroup( "Waves,10/B,30" ); >;
    float g_flStrengthC < Default( 0.025 ); Range( 0, 1 ); UiGroup( "Waves,10/C,30" ); >;
    float g_flStrengthD < Default( 0.0125 ); Range( 0, 1 ); UiGroup( "Waves,10/D,30" ); >;

    //
    // Fog
    //
    float g_flFogDistance < UiType( Slider ); Range( 0, 512 ); Default( 1.0 ); UiGroup( "Fog,300/30" ); >;
    float g_flFogStrength < UiType( Slider ); Range( 0, 1 ); Default( 1.0 ); UiGroup( "Fog,300/30" ); >;
    float3 g_vColor < UiType( Color ); Default3( 1.0, 1.0, 1.0 ); UiGroup( "Fog,300/20" ); >;
    float3 g_vDepthColor < UiType( Color ); Default3( 1.0, 1.0, 1.0 ); UiGroup( "Fog,300/20" ); >;

    //
    // Caustics
    //
    float g_flCausticsScale < UiType( Slider ); Range( 0.25, 4 ); Default( 1.0 ); UiGroup( "Caustics,10/30" ); >;
    float g_flCausticsStrength < UiType( Slider ); Range( 0, 4 ); Default( 1.0 ); UiGroup( "Caustics,10/30" ); >;
    float g_flCausticsBlur < UiType( Slider ); Range( 0, 1 ); Default( 0.0 ); UiGroup( "Caustics,10/30" ); >;

    CreateInputTexture2D( Caustics, Linear, 8, "", "_caustics_flipbook", "Caustics,10/10", Default3( 1.0, 1.0, 1.0 ) );
    Texture2D g_tCaustics < Channel( RGB, Box( Caustics ), Linear ); OutputFormat( BC7 ); SrgbRead( false ); >;
    
    float g_flFlipbookFramerate < UiType( Slider ); Range( 1, 60 ); Default( 24 ); UiGroup( "Caustics,10/Flipbook,20" ); >;
    float g_flFlipbookBlend < UiType( Slider ); Range( 0, 1 ); Default( 0.5 ); UiGroup( "Caustics,10/Flipbook,20" ); >;
    bool g_bFlipbookLoop < Default( 1 ); UiGroup( "Caustics,10/Flipbook,20" ); >;
    bool g_bFlipbookInterpolate < Default( 1 ); UiGroup( "Caustics,10/Flipbook,20" ); >;
    int g_nFlipbookRows < Default( 4 ); UiGroup( "Caustics,10/Flipbook,20" ); >;
    int g_nFlipbookColumns < Default( 4 ); UiGroup( "Caustics,10/Flipbook,20" ); >;
    
    //
    // Oily sheen
    //
    float g_flOilSheen < UiType( Slider ); Range( 0, 1 ); Default( 0.5 ); UiGroup( "Oily Sheen,10/30" ); >;
    float g_flOilRainbowIntensity < UiType( Slider ); Range( 0, 1 ); Default( 0.7 ); UiGroup( "Oily Sheen,10/30" ); >;
    float g_flOilScale < UiType( Slider ); Range( 0.1, 10 ); Default( 2.0 ); UiGroup( "Oily Sheen,10/30" ); >;
    float g_flOilSpeed < UiType( Slider ); Range( 0, 2 ); Default( 0.2 ); UiGroup( "Oily Sheen,10/30" ); >;

	//
	// Engine
	//
    BoolAttribute( bWantsFBCopyTexture, true );
    BoolAttribute( translucent, true );
    CreateTexture2D( g_tFrameBufferCopyTexture ) < Attribute("FrameBufferCopyTexture");   SrgbRead( true ); Filter(MIN_MAG_MIP_LINEAR);    AddressU( CLAMP );     AddressV( CLAMP ); >;

    float GetMul( float2 vTextureCoords )
    {
        float f = length( fwidth( vTextureCoords ) ) * 1.0;
        f = 1.0 - pow( saturate( f ), 0.1 );
        return float4( f.xxx, 1.0 );
    }
    
    float3 OilSheen(float3 normal, float3 viewDir, float scale, float time, float intensity)
    {
        // Viewing angle
        float NdotV = saturate(dot(normal, viewDir));
        float fresnel = pow(1.0 - NdotV, 5.0);
        
        // Phase shift based on view normal
        float phaseShift = dot(normal.xy, float2(0.5, 0.5)) * 3.0;
        
        float thickness = sin(phaseShift + time * g_flOilSpeed) * 0.5 + 0.5;

        scale *= 10;
        
        float3 rainbow;
        rainbow.r = sin(thickness * 6.28 * scale + 0.0) * 0.5 + 0.5;
        rainbow.g = sin(thickness * 6.28 * scale + 2.09) * 0.5 + 0.5;
        rainbow.b = sin(thickness * 6.28 * scale + 4.19) * 0.5 + 0.5;
        
        return rainbow * intensity * fresnel;
    }

    // Flipbook sampling function with frame interpolation
    float3 SampleFlipbook(Texture2D flipbookTexture, SamplerState samplerState, float2 uv, float time, float fps, bool loop, bool interpolate)
    {
        // Constants for 4x4 flipbook
        const int ROWS = g_nFlipbookRows;
        const int COLUMNS = g_nFlipbookColumns;
        const int TOTAL_FRAMES = ROWS * COLUMNS;
        
        // Calculate frame indices
        float frameTime = time * fps;
        int frameIndex = floor(frameTime);
        
        if (loop)
        {
            frameIndex = frameIndex % TOTAL_FRAMES;
        }
        else
        {
            frameIndex = min(frameIndex, TOTAL_FRAMES - 1);
        }
        
        // Get next frame for interpolation
        int nextFrameIndex = (frameIndex + 1) % TOTAL_FRAMES;
        if (!loop && frameIndex == TOTAL_FRAMES - 1)
        {
            nextFrameIndex = frameIndex;
        }
        
        // Calculate UV coordinates for current frame
        int currentRow = frameIndex % COLUMNS;
        int currentCol = frameIndex / COLUMNS;
        
        float2 frameSize = float2(1.0f / COLUMNS, 1.0f / ROWS);
        float2 currentFrameOffset = float2(currentCol * frameSize.x, currentRow * frameSize.y);
        
        // Calculate UV coordinates for next frame
        int nextRow = nextFrameIndex % COLUMNS;
        int nextCol = nextFrameIndex / COLUMNS;
        float2 nextFrameOffset = float2(nextCol * frameSize.x, nextRow * frameSize.y);
        
        // Scale UV to frame size and add offset
        float2 frameUV = frac(uv) * frameSize;
        float2 currentUV = currentFrameOffset + frameUV;
        float2 nextUV = nextFrameOffset + frameUV;
        
        // Sample current and next frames
        float3 currentFrame = flipbookTexture.SampleLevel(samplerState, currentUV, 0).rgb;
        float3 nextFrame = flipbookTexture.SampleLevel(samplerState, nextUV, 0).rgb;
        
        // Interpolate between frames if enabled
        if (interpolate)
        {
            float blend = frac(frameTime);
            return lerp(currentFrame, nextFrame, blend);
        }
        else
        {
            return currentFrame;
        }
    }

#if S_PROCEDURAL
    //
    // Layered fractal noise
    //
    float FractalNoise( float2 p ) {
        return Simplex2D( p ) * 0.5333
            + Simplex2D( p * 2.0 ) * 0.2667
            + Simplex2D( p * 4.0 ) * 0.1333
            + Simplex2D( p * 8.0 ) * 0.0667;
    }

    //
    // Sample layered noise
    //
    float Noise( float2 v )
    {
        float fNoiseA = FractalNoise( v * 0.1 );

        v *= float2( 0.9, 1 );
        float fNoiseB = FractalNoise( v * 0.1 * 2.0 );

        v *= float2( 0.9, 1 );
        float fNoiseC = FractalNoise( v * 0.1 * 4.0 );

        v *= float2( 0.9, 1 );
        float fNoiseD = FractalNoise( v * 0.1 * 8.0 );

        float fNoise = (fNoiseA * fNoiseB + fNoiseC * fNoiseD) * 0.5;

        return fNoise * 0.5 + 0.5;
    }

    //
    // Mixed/blended and animated wave normal maps
    //
    float3 GetNormalForPixel( float3 normal, float2 pixel )
    {
        float fMul = GetMul( pixel );

        float scaleA = g_flStrengthA.x;
        float scaleB = g_flStrengthB.x;
        float scaleC = g_flStrengthC.x;
        float scaleD = g_flStrengthD.x;

        scaleB *= fMul;
        scaleC *= fMul / 8;
        scaleD *= fMul / 16;

        scaleA = saturate( scaleA );
        scaleB = saturate( scaleB );
        scaleC = saturate( scaleC );
        scaleD = saturate( scaleD );

        float3 baseNormal = float3( 0, 0, 1 );

        const float fFrequencies[] = { g_flFrequencyA, g_flFrequencyB, g_flFrequencyC, g_flFrequencyD };
        const float fStrengths[] = { scaleA, scaleB, scaleC, scaleD };
        const float2 fSpeeds[] = { g_flSpeedA, g_flSpeedB, g_flSpeedC, g_flSpeedD };

        for ( int i = 0; i < g_nFrequencyCount; i++ )
        {
            float fFrequency = fFrequencies[ i ];
            float fStrength = fStrengths[ i ];
            float2 fSpeed = fSpeeds[ i ];
            
            float2 uv = pixel.xy + ( fSpeed * float2( g_flTime, g_flTime ) );

            float2 eps = float2( 0.0001, 0.0 );
            float h = Noise( uv * fFrequency );
            float hx = Noise( ( uv + eps.xy ) * fFrequency );
            float hy = Noise( ( uv + eps.yx ) * fFrequency );

            float3 dpdx = float3( 1.0, 0.0, ( hx - h ) / eps.x );
            float3 dpdy = float3( 0.0, 1.0, ( hy - h ) / eps.x );

            float3 perturbedNormal = normalize( cross( dpdx, dpdy ) );
            baseNormal = normalize( lerp( baseNormal, perturbedNormal, fStrength ) );
        }

        baseNormal = baseNormal / g_nFrequencyCount;

        float3 vDefault = float3( 0, 0, 1 );
        float3 vNormal = normalize( lerp( vDefault, baseNormal, g_flNormalScale ) );
        return vNormal;
    }
#else
	//
	// Mixed/blended and animated wave normal maps
	//
    float3 GetNormalForPixel( float3 normal, float2 pixel )
    {
        float fMul = GetMul( pixel );

        float scaleA = g_flScaleA.x;
        float scaleB = g_flScaleB.x;
        scaleB *= fMul;
        
        scaleA = saturate( scaleA );
        scaleB = saturate( scaleB );

        float2 pixelA = pixel.xy + ( g_flSpeedA * float2( g_flTime, g_flTime ) );
        float2 pixelB = pixel.xy + ( g_flSpeedB * float2( g_flTime, g_flTime ) );

        pixelA *= scaleA;
        pixelB *= scaleB;

        float3 a = DecodeNormal( g_tNormalMapA.Sample( g_sSampler, pixelA.xy ).xyz );
        float3 b = DecodeNormal( g_tNormalMapB.Sample( g_sSampler, pixelB.xy ).xyz );

        // Standard RNM formula (Mikkelsen version)
        float3 r = normalize( a + b - float3( 0,0,1 ) * dot( a, float3( b.xy, -b.z ) ) );

        float3 vDefault = float3( 0, 0, 1 );
        float3 vNormal = normalize( lerp( vDefault, r, g_flNormalScale ) );
        return vNormal;
    }
#endif
    
    //
    // Fetch the sun light index
    //
    int GetSunLightIndex()
    {
        for ( uint nLightIdx = 0; nLightIdx < NumDynamicLights; nLightIdx++ )
        {
            BinnedLight light = BinnedLightBuffer[ nLightIdx ];

            // Hacky
            if ( length( light.GetPosition() ) < 10000.0f )
                continue;

            return nLightIdx;
        }

        return 0;
    }

	//
	// Sample direct lighting from the sun on a given point
	//
	float SampleLightDirect( int nSunLightIdx, float3 vPosWs )
	{
        float fLight = 0.0;
        float lightContribution = 1.0;
        BinnedLight sun = BinnedLightBuffer[ nSunLightIdx ];
            
        for ( uint nFrustaIdx = 0; nFrustaIdx < sun.NumShadowFrusta(); nFrustaIdx++ )
        {
            lightContribution *= ComputeShadow( vPosWs, sun.WorldToShadow[ nFrustaIdx ], sun.ShadowBounds[ nFrustaIdx ] );
        }
        
        fLight += lightContribution;
        return saturate( fLight / float( NumDynamicLights ) );
	}

    //
    // Calculate the luminance of a color
    //
    float luminance( float3 color )
    {
        return dot( color, float3( 0.2126, 0.7152, 0.0722 ) );
    }
    
    float CalculateSpecular( float3 vNormal, float3 vViewDir, float3 vLightDir )
    {
        float3 R = reflect( -vLightDir, vNormal );
        float fSpecular = pow( saturate( dot( R, vViewDir ) ), 1000.0 );
        return fSpecular;
    }

    float CalculateSpecularAll( float3 vNormal, float3 vViewDir )
    {
        float fSpecular = 0.0;
        
        for ( uint nLightIdx = 0; nLightIdx < NumDynamicLights; nLightIdx++ )
        {
            BinnedLight light = BinnedLightBuffer[ nLightIdx ];
            float3 vLightDir = normalize( light.GetPosition() );
            fSpecular += CalculateSpecular( vNormal, vViewDir, vLightDir );
        }

        return fSpecular / float( NumDynamicLights );
    }
    
    float4 MainPs( PixelInput i ) : SV_Target0
    {
        //
        // Material
        //
        float3 vColor = float3( 0, 0, 0 );
        float3 vEmission = float3( 0, 0, 0 );
        float3 vNormal = float3( 0, 0, 1 );

		i.vTextureCoords *= pow( 2, g_flTilingScale );
        
		//
		// Gather everything we need
		//
        int nSunLightIdx = GetSunLightIndex();
        float3 vPositionWs = i.vPositionWithOffsetWs.xyz + g_vHighPrecisionLightingOffsetWs.xyz;
        float2 vTextureCoords = vPositionWs.xy / ( g_flScale.xx * 20.0 );
		vNormal = GetNormalForPixel( i.vNormalWs, vTextureCoords.xy );
        
        float3 vScenePositionWs = Depth::GetWorldPosition( i.vPositionSs.xy );
        
        float fFeatherDepth = ( 1.0f - ( vScenePositionWs.z - vPositionWs.z ) ) / g_flFeatherDistance;
        fFeatherDepth = saturate( fFeatherDepth );
        
        float2 vDitherCoord = mad( i.vPositionSs.xy, ( 1.0f / 256.0f ), g_vRandomFloats.xy );
        float2 vNoise = float2( 
            g_tBlueNoise.Sample( g_sSampler, vDitherCoord ).r,
            g_tBlueNoise.Sample( g_sSampler, vDitherCoord ).g
        );

        //
        // Refraction
        //
        float refractionStrength = g_flRefractionScale * fFeatherDepth;
        refractionStrength = saturate( refractionStrength );

        float2 vRefractOffset = vNormal * refractionStrength * length( g_vViewportSize );
        float2 vRefractPositionUv = i.vPositionSs.xy + vRefractOffset;
        
        float2 edgeDistance = min( vRefractPositionUv, g_vViewportSize - vRefractPositionUv ) / ( refractionStrength * 50.0 );
        float edgeFade = saturate( min( edgeDistance.x, edgeDistance.y ) );
        
        if ( g_bRefractionDepthTest )
        {
            // Sample scene depth against surface depth, if scene depth is greater then the refraction is invalid
            float fSceneDepth = Depth::Get( vRefractPositionUv );
            float fSurfaceDepth = i.vPositionSs.z / g_flViewportMaxZ;
            float fMul = 0;

            if ( fSurfaceDepth > fSceneDepth )
                fMul = 1;

            edgeFade *= fMul;
        }

        vRefractOffset *= edgeFade;
        vRefractPositionUv = i.vPositionSs.xy + vRefractOffset;

        float3 vRefractPositionWs = Depth::GetWorldPosition( vRefractPositionUv );

        float2 vRefractionUv = vRefractPositionUv * g_vFrameBufferCopyInvSizeAndUvScale.xy;
        vRefractionUv = clamp( vRefractionUv, 0.0, g_vFrameBufferCopyInvSizeAndUvScale.zw );
		float3 vClearRefracted = g_tFrameBufferCopyTexture.SampleLevel( g_sClampSampler, vRefractionUv, g_nBlurLevel ).xyz;
        vClearRefracted += ddx( vClearRefracted.xyz ) * float3( 1, 0, -1 );

        #if S_INDIRECT > 0
            #if S_INDIRECT == 1
                const int nSampleCount = 1;
            #elif S_INDIRECT == 2
                const int nSampleCount = 3;
            #elif S_INDIRECT == 3
                const int nSampleCount = 16;
            #endif
            const float fScale = 128.0;
            float fLightSurface = 0.0;

            for ( int nSample = 0; nSample < nSampleCount; ++nSample )
            {
                float2 vDitherCoord = ( i.vPositionSs.xy + ( nSample * 50 ) ) / 256.0f;
                float2 vOffset = float2(
                    g_tBlueNoise.Sample( g_sSampler, vDitherCoord ).r,
                    g_tBlueNoise.Sample( g_sSampler, vDitherCoord ).g
                );

                vOffset *= fScale;
                float3 vSamplePosition = vPositionWs + float3( vOffset, 0 );
                
                fLightSurface += SampleLightDirect( nSunLightIdx, vSamplePosition ) / nSampleCount;
            }

            vClearRefracted = lerp( vClearRefracted * 1.5f, vClearRefracted, 1.0f - fLightSurface );
        #endif

        //
        // Basic depth fog
        //
        float fFog = ( 1.0f - ( vRefractPositionWs.z - vPositionWs.z ) ) / g_flFogDistance;
        fFog = saturate( fFog );
        fFog *= g_flFogStrength;

        float3 vTint = g_vColor;
        vColor = lerp( vClearRefracted * vTint, g_vDepthColor, fFog );
        
        //
        // Project caustics
        //
        #if S_CAUSTICS > 0
            float fLightScene = SampleLightDirect( nSunLightIdx, vRefractPositionWs );

            float4x4 lightMatrix = BinnedLightBuffer[ nSunLightIdx ].WorldToShadow[ 0 ];
            float4 vPositionTextureSpace = mul( float4( vRefractPositionWs + g_vHighPrecisionLightingOffsetWs, 1.0 ), lightMatrix );
            //float3 vWorld = vPositionTextureSpace.xyz * 10.0;
            float3 vWorld = vRefractPositionWs / 100.0;
            float2 vCausticsTexCoord = vWorld.xy;
            vCausticsTexCoord += vNoise * g_flCausticsBlur * 0.1;
            
            #if S_CAUSTICS == 1
                float fCaustics = g_tCaustics.Sample( g_sSampler, vCausticsTexCoord ).x;
            #else
                float fCaustics = SampleFlipbook( g_tCaustics, g_sSampler, vCausticsTexCoord, g_flTime, g_flFlipbookFramerate, g_bFlipbookLoop, g_bFlipbookInterpolate ).x;
            #endif

            float3 vCaustics = fCaustics.xxx;
            vCaustics += ddx( vCaustics.xyz ) * float3( 3, 0, -3 );
            vCaustics = saturate( vCaustics );
            vCaustics *= fLightScene;

            vEmission = vCaustics * fLightScene * fFeatherDepth * g_flCausticsStrength * ( 1.0f - fFog );
        #endif
        
        //
        // Oily sheen
        //
        if ( g_bSheen )
        {
            float3 vViewDirWs = normalize( g_vCameraPositionWs - vPositionWs );
            float3 vOilSheen = OilSheen( vNormal, vViewDirWs, g_flOilScale, g_flTime, g_flOilRainbowIntensity );
            vColor = lerp( vColor, vColor + vOilSheen, g_flOilSheen );
        }
        
        //
        // Basic waterline
        //
        if ( g_bWaterline )
        {
		    float fDepth = Depth::GetNormalized( vRefractPositionUv );
		    float fInvCameraToDepth = -abs( dot( i.vPositionWithOffsetWs, -g_vCameraDirWs ) ) + ( 1 / fDepth );
			float fWaterFeathering = saturate( ( fInvCameraToDepth * 0.5 ) );
            vColor.rgb = lerp( vColor.rgb, vColor.rgb * 2.0, 1.0f - fWaterFeathering );
        }
        
        //
        // Envmap reflections
        //
        float3 vReflection = EnvMap::From( vPositionWs, i.vPositionSs, vNormal, 0 );

        //
        // Mix with dynamic reflections if enabled
        //
        #if S_DYNAMIC_REFLECTIONS > 0
            const float3 vRayWs = CalculateCameraToPositionDirWs( vPositionWs );

            float3 vReflectWs = reflect( vRayWs, vNormal );
            float2 vPositionSs = i.vPositionSs.xy;

            TraceResult trace = ScreenSpace::Trace( vPositionWs, vReflectWs, vPositionSs, 64, g_nBlurLevel );
            float2 uv = saturate( trace.HitClipSpace.xy * g_vFrameBufferCopyInvSizeAndUvScale.zw );

            // Composite result
            #if S_DYNAMIC_REFLECTIONS == 1
                float3 vReflectionColor = g_tFrameBufferCopyTexture.SampleLevel( g_tFrameBufferCopyTexture_sampler, uv, g_nBlurLevel ).rgb;
                
                vReflection = lerp( vReflection, vReflectionColor, saturate( trace.Confidence ) );
            #elif S_DYNAMIC_REFLECTIONS == 2
                float3 vReflectionColor = g_tFrameBufferCopyTexture.SampleLevel( g_tFrameBufferCopyTexture_sampler, uv, g_nBlurLevel ).rgb;

                float2 dx = ddx(trace.HitClipSpace.xy);
                float2 dy = ddy(trace.HitClipSpace.xy);

                float3 vReflectionColorDx = g_tFrameBufferCopyTexture.SampleLevel( g_tFrameBufferCopyTexture_sampler, uv + dx, g_nBlurLevel ).rgb;
                float3 vReflectionColorDy = g_tFrameBufferCopyTexture.SampleLevel( g_tFrameBufferCopyTexture_sampler, uv + dy, g_nBlurLevel ).rgb;
                float3 vReflectionColorDxy = g_tFrameBufferCopyTexture.SampleLevel( g_tFrameBufferCopyTexture_sampler, uv + dx + dy, g_nBlurLevel ).rgb;

                float3 vReflectionColorAA = (vReflectionColor + vReflectionColorDx + vReflectionColorDy + vReflectionColorDxy) / 4.0;

                vReflection = lerp( vReflection, vReflectionColorAA, saturate( trace.Confidence ) );
            #endif
        #endif
        
        float3 vViewDir = normalize( g_vCameraPositionWs - vPositionWs );
        float fFresnel = saturate( 1 - dot( vViewDir, vNormal ) );
        fFresnel = pow( fFresnel, 5 );
        fFresnel = saturate( fFresnel );
        
        vColor = lerp( vColor, vReflection, saturate( fFresnel ) );

        return float4( vColor.rgb + vEmission.rgb, 1.0 );
    }
}