uniform float light = 1.0;
uniform sampler2D black_render_pass;
uniform sampler2D previous_pass;

in vec2 tex_coord;
out vec4 color;


void main(){
  float density = max(0.5, 1.0*(1-light));
  float weight = max(0.02, 0.2*(1-light));
  float exposure = 1.0;
  int samples = 150;
  vec2 origin = vec2(0.5, 0.3);
  
  vec2 size = textureSize(previous_pass, 0);
  float shadow_intensity = 0.0;
  vec2 delta_coord = vec2(tex_coord - origin);
  float dist = length(delta_coord*size/1000/2048/size.x);

  vec2 _tex_coord = tex_coord;
  delta_coord *= (1.0 /  float(samples)) * density;

  for(int i=0; i < samples; i++){
    _tex_coord -= delta_coord;
    float samp = texture(black_render_pass, _tex_coord).a;
    samp *= weight;
    shadow_intensity += samp;
  }
  
  shadow_intensity *= exposure * (dist/5);
  vec4 previous = texture(previous_pass, tex_coord);
  color = previous;
  color.rgb *= 1-shadow_intensity;
  color.rgb += 0.2*(1-dist)*light;
  color.rgb *= min(1.0, light+0.2);
  if(0.3 < previous.r) color.r = mix(color.r, previous.r, 0.5);
  if(0.3 < previous.g) color.g = mix(color.g, previous.g, 0.5);
  if(0.3 < previous.b) color.b = mix(color.b, previous.b, 0.5);
  color.a = 1;
}
