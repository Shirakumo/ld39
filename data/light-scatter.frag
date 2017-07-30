uniform float density = 0.5;
uniform float weight = 0.05;
uniform float decay = 1.0;
uniform float exposure = 1.0;
uniform int samples = 150;
uniform vec2 origin = vec2(0.5, 0.3);
uniform sampler2D black_render_pass;
uniform sampler2D previous_pass;

in vec2 tex_coord;
out vec4 color;


void main(){
  vec2 size = textureSize(previous_pass, 0);
  float shadow_intensity = 0.0;
  vec2 delta_coord = vec2(tex_coord - origin);
  float dist = length(delta_coord*size/500/2048/size.x);

  vec2 _tex_coord = tex_coord;
  delta_coord *= (1.0 /  float(samples)) * density;
  float illumination_decay = 1.0;

  for(int i=0; i < samples; i++){
    _tex_coord -= delta_coord;
    float samp = texture(black_render_pass, _tex_coord).a;
    samp *= illumination_decay * weight;
    shadow_intensity += samp;
    illumination_decay *= decay;
  }
  
  shadow_intensity *= exposure * (dist/5);
  color = texture(previous_pass, tex_coord);
  color.xyz *= (1-clamp(shadow_intensity, 0.0, 0.7));
  color.xyz += 0.2*(1-dist);
  color.a = 1;
}
