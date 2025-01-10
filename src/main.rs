use std::{collections::HashMap, ffi::CString, sync::Mutex};

use glfw::{self, Action, Context, Key};

use gl::{self, types::*};

use nalgebra as na;

use nalgebra_glm as glm;


const BASE_WIDTH: u32 = 800;
const BASE_HEIGHT: u32 = 600;

#[derive(Copy, Clone)]
struct Position(glm::Vec3);

#[derive(Debug)]
struct Character {
    texture_id: u32,
    advance: u32,
    size: na::Vector2<i32>,
    bearing: na::Vector2<i32>
}

struct Viewport {
    width: u32,
    height: u32
}

impl Viewport {
    const fn new(w: u32, h: u32) -> Self {
        Self {
            width: w,
            height: h
        }
    }

    fn set(&mut self, w: u32, h: u32) {
        self.width = w;
        self.height = h;
    }

    fn width(&self) -> u32 {
        self.width
    }

    fn height(&self) -> u32 {
        self.height
    }

    unsafe fn bind(&self) {
        gl::Viewport(0, 0, self.width as i32, self.height as i32);
    }
} 

const FONT_VERTEX: &str = r#"
#version 450 core
layout (location = 0) in vec4 vertex;
out vec2 TexCoords;

uniform mat4 projection;

void main() {
    gl_Position = projection * vec4(vertex.xy, 0.0, 1.0f);
    TexCoords = vertex.zw;
}
"#;

const FONT_FRAGMENT: &str = r#"
#version 450 core
in vec2 TexCoords;
out vec4 color;

uniform sampler2D text;
uniform vec3 textColor;

void main() {
    vec4 sampled = vec4(1.0, 1.0, 1.0, texture(text, TexCoords).r);
    color = vec4(textColor, 1.0) * sampled;
}
"#;

const OBJ_VERTEX: &str = r#"
#version 450 core
layout (location = 0) in vec2 pos;

uniform mat4 projection;
uniform mat4 model;

void main() {
    gl_Position = projection * model * vec4(pos, 0.0, 1.0);
}
"#;

const OBJ_FRAGMENT: &str = r#"
#version 450 core

out vec4 color;

void main() {
    color = vec4(1.0f);
}
 
"#;

#[cfg(debug_assertions)]
macro_rules! gl_assert {
    () => {
        match gl::GetError() {
            0 => (),
            err => { dbg!(err); },
        }
    }
}

static VIEWPORT: Mutex<Viewport> = Mutex::new(Viewport::new(BASE_WIDTH, BASE_HEIGHT));

fn main() {
    use glfw::fail_on_errors;
    let mut glfw = glfw::init(fail_on_errors!()).expect("Fail to init glfw");
    
    glfw.window_hint(glfw::WindowHint::ContextVersion(4, 5));
    glfw.window_hint(glfw::WindowHint::OpenGlProfile(glfw::OpenGlProfileHint::Core));

    let (mut window, events) = glfw.create_window(BASE_WIDTH, BASE_HEIGHT, "Pong", glfw::WindowMode::Windowed).expect("Failed to create window");

    window.make_current();
    window.set_key_polling(true);
    
    gl::load_with(|s| window.get_proc_address(s));

    unsafe{ gl_assert!() } ;

    
    window.set_framebuffer_size_callback(|_w, width, height| {
        let mut vp = VIEWPORT.lock().unwrap();
        vp.set(width as u32 , height as u32);
        unsafe {
            vp.bind();
        }
    });

    unsafe {
        gl::Enable(gl::BLEND);
        gl::BlendFunc(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);
        VIEWPORT.lock().unwrap().bind();
        gl::PixelStorei(gl::UNPACK_ALIGNMENT, 1);
        gl_assert!();
    }

    let mut characters: HashMap<char, Character> = HashMap::new();


    gen_font(&mut characters);
    
    let projection = na::Matrix4::new_orthographic(0.0, 800.0, 0.0, 600.0, 0.0, 1.0);

    let shader;
    unsafe {
        shader = gl::CreateProgram();
        //
        let vs = gl::CreateShader(gl::VERTEX_SHADER);
        let vertex_source = CString::new(FONT_VERTEX).unwrap();
        gl::ShaderSource(vs, 1, &vertex_source.as_ptr().cast(), std::ptr::null());
        gl::CompileShader(vs);
        check_shader_error("vertex", vs);
        gl::AttachShader(shader, vs);

        let fs = gl::CreateShader(gl::FRAGMENT_SHADER);
        let frag_source = CString::new(FONT_FRAGMENT).unwrap();
        gl::ShaderSource(fs, 1, &frag_source.as_ptr().cast(), std::ptr::null());
        gl::CompileShader(fs);
        check_shader_error("fragment", fs);
        gl::AttachShader(shader, fs);

        gl::LinkProgram(shader);
        check_program_error(shader);

        gl::UseProgram(shader);
        let s = CString::new("projection").unwrap();
        gl::UniformMatrix4fv(gl::GetUniformLocation(shader, s.as_ptr()), 1, gl::FALSE, projection.as_ptr());
    }

    // Start screen
    let mut vao = 0;
    let mut vbo = 0;

    unsafe {
        gl::CreateVertexArrays(1, &mut vao);
        gl::CreateBuffers(1, &mut vbo);

        gl::NamedBufferData(vbo, (4 * 6 * std::mem::size_of::<f32>()) as _, std::ptr::null(), gl::DYNAMIC_DRAW);

        gl::VertexArrayVertexBuffer(vao, 0, vbo, 0, (4 * std::mem::size_of::<f32>()) as _);

        gl::EnableVertexArrayAttrib(vao, 0);

        gl::VertexArrayAttribFormat(vao, 0, 4, gl::FLOAT, gl::FALSE, 0);

        gl::VertexArrayAttribBinding(vao, 0, 0);

    }

    let mut game_start = false;

    // Objects
    
    let mut bvao = 0;
    let mut bvbo = 0;

    let quad: [f32; 8] = [
        0.0, 0.0,
        0.5, 0.0,
        0.5, 0.5,
        0.0, 0.5,
    ];

    let indices: [u32; 6] = [
        0, 1, 3,
        1, 2, 3
    ];

    let mut ebo = 0;
    unsafe {
        gl::CreateVertexArrays(1, &mut bvao);
        gl::CreateBuffers(1, &mut bvbo);
        gl::CreateBuffers(1, &mut ebo);

        assert_ne!(bvbo, 0);
        assert_ne!(bvao, 0);
        assert_ne!(ebo, 0);
        
        gl::NamedBufferData(bvbo, std::mem::size_of_val(&quad) as _, quad.as_ptr().cast(), gl::STATIC_DRAW);
        gl::NamedBufferData(ebo, std::mem::size_of_val(&indices) as _, indices.as_ptr().cast(), gl::STATIC_DRAW);

        gl::VertexArrayVertexBuffer(bvao, 0, bvbo, 0, 2 * std::mem::size_of::<f32>() as GLsizei);

        gl::VertexArrayElementBuffer(bvao, ebo);

        gl::EnableVertexArrayAttrib(bvao, 0);
        
        gl::VertexArrayAttribFormat(bvao, 0, 2, gl::FLOAT, gl::FALSE, 0);

        gl::VertexArrayAttribBinding(bvao, 0, 0);
    }
    
    let obj_shader;
    unsafe {
        obj_shader = gl::CreateProgram();
        //
        let vs = gl::CreateShader(gl::VERTEX_SHADER);
        let vertex_source = CString::new(OBJ_VERTEX).unwrap();
        gl::ShaderSource(vs, 1, &vertex_source.as_ptr().cast(), std::ptr::null());
        gl::CompileShader(vs);
        check_shader_error("vertex", vs);
        gl::AttachShader(obj_shader, vs);

        let fs = gl::CreateShader(gl::FRAGMENT_SHADER);
        let frag_source = CString::new(OBJ_FRAGMENT).unwrap();
        gl::ShaderSource(fs, 1, &frag_source.as_ptr().cast(), std::ptr::null());
        gl::CompileShader(fs);
        check_shader_error("fragment", fs);
        gl::AttachShader(obj_shader, fs);

        gl::LinkProgram(obj_shader);
        check_program_error(obj_shader);

        let proj = glm::ortho(-1.0, 1.0, -1.0, 1.0, 0.0, 1.0);

        gl::UseProgram(obj_shader);
        let s = CString::new("projection").unwrap();
        gl::UniformMatrix4fv(gl::GetUniformLocation(obj_shader, s.as_ptr()), 1, gl::FALSE, proj.as_ptr());
     
    }

    let circle = gen_circle(glm::vec2(0.0, 0.0), 0.1);
    let mut cvao = 0;
    let mut cvbo = 0;
    
    unsafe {
        gl::CreateVertexArrays(1, &mut cvao);
        gl::CreateBuffers(1, &mut cvbo);

        assert_ne!(cvbo, 0);
        assert_ne!(cvao, 0);
        
        gl::NamedBufferData(cvbo, std::mem::size_of_val(circle.as_slice()) as _, circle.as_ptr().cast(), gl::STATIC_DRAW);

        gl::VertexArrayVertexBuffer(cvao, 0, cvbo, 0, 2 * std::mem::size_of::<f32>() as GLsizei);

        gl::EnableVertexArrayAttrib(cvao, 0);
        
        gl::VertexArrayAttribFormat(cvao, 0, 2, gl::FLOAT, gl::FALSE, 0);

        gl::VertexArrayAttribBinding(cvao, 0, 0);
    }


    let mut winner = false;
    let mut player1 = Position(glm::Vec3::new(-0.9, -0.2, 0.0));
 
    let mut player2 = Position(glm::Vec3::new(0.9, -0.2, 0.0));

    let mut ballpos = Position(glm::Vec3::new(0.0, 0.0, 0.0));
    let mut balldirection = glm::Vec2::new(1.0, 0.0);

    let mut score1 = 0;
    let mut score2 = 0;

    let mut current_time = glfw.get_time();
    let mut last_time = current_time;
    let mut delta;

    while !window.should_close() {
        delta = current_time - last_time;
        last_time = current_time;
        current_time = glfw.get_time();

        if ballpos.0.x > 1.0 || ballpos.0.x < -1.0 {
            if ballpos.0.x > 1.0 {
                score1 += 1;
            } else {
                score2 += 1;
            }

            ballpos.0.x = 0.0;
            ballpos.0.y = 0.0;
            ballpos.0.z = 0.0;
            balldirection.x = -(balldirection.x); 
            balldirection.y = 0.0;

            player1 = Position(glm::vec3(-0.9, -0.2, 0.0));
            player2 = Position(glm::vec3(0.9, -0.2, 0.0));
        }

        if check_collision(player1, glm::Vec2::new(0.08, 0.4), ballpos, glm::Vec2::new(0.02, 0.02)) {

            let p1dir = glm::vec2(1.0, 0.0);
            let angle = glm::angle(&balldirection, &p1dir);
            if ballpos.0.y < player1.0.y + 0.2 + 0.08 && ballpos.0.y > player1.0.y + 0.2 - 0.08 {
                balldirection.x *= -1.0;
            } else {
                if angle > -1.0 && angle < 1.0 {
                    let dist = ballpos.0.y  - player1.0.y;
                    balldirection.y = (1.0/dist).tan();
                } else {
                    balldirection.y = 0.0;
                }
                balldirection.x *= -1.0;
            }
        } 

        if check_collision(player2, glm::Vec2::new(0.05, 0.4), ballpos, glm::Vec2::new(0.02, 0.02)) {
            let p2dir = glm::vec2(1.0, 0.0);
            let angle = glm::angle(&p2dir, &balldirection);
            // Inside center
            if ballpos.0.y < player2.0.y + 0.2 + 0.08 && ballpos.0.y > player2.0.y + 0.2 - 0.08 {
                balldirection.x *= -1.0;
            } else {
                if angle > -1.0 && angle < 1.0 {
                    let dist = ballpos.0.y  - player2.0.y;
                    balldirection.y = (1.0/dist).tan();
                } else {
                    balldirection.y = 0.0;
                }
                balldirection.x *= -1.0;
            }
        }


        if ballpos.0.y > 0.92 || ballpos.0.y < -0.92 {
            balldirection.y = -balldirection.y;
        }

        if score1 >= 10 || score2 >= 10 {
            winner = true;
        }

        window.swap_buffers();

        unsafe {
            gl::Clear(gl::COLOR_BUFFER_BIT);
            gl::ClearColor(0.1, 0.1, 0.1, 1.0);
        }

        if !game_start {
            render_text(&characters, shader, vao, vbo, "Pong - Demo", 0.36, 0.6, 2.0, na::Vector3::new(1.0, 1.0, 1.0));
            render_text(&characters, shader, vao, vbo, "press ENTER to start.",
                0.36, 0.5, 1.0, na::Vector3::new(1.0, 1.0, 1.0)
            );
        } else if winner {
            render_text(&characters, shader, vao, vbo, "We have a winner!", 0.30, 0.6, 2.0, na::Vector3::new(1.0, 1.0, 1.0));
            let name;
            if score1 > score2 {
                name = "Player 1 Wins!";
            } else {
                name = "Player 2 Wins!";
            }
            render_text(&characters, shader, vao, vbo, name,
                0.4, 0.5, 1.0, na::Vector3::new(1.0, 1.0, 1.0)
            );

        } else {
            unsafe {
                render_text(&characters, shader, vao, vbo, &score1.to_string(), 0.2, 0.8, 4.0, na::Vector3::new(1.0, 1.0, 1.0));
                render_text(&characters, shader, vao, vbo, &score2.to_string(), 0.8, 0.8, 4.0, na::Vector3::new(1.0, 1.0, 1.0));

                gl::UseProgram(obj_shader);
                gl::BindVertexArray(bvao);
                gl_assert!();

                let mut p1model = glm::Mat4::identity();
                p1model = glm::translate(&p1model, &player1.0);
                p1model = glm::scale(&p1model, &glm::vec3(0.1, 0.8, 1.0));

                let s = CString::new("model").unwrap();
                gl::UniformMatrix4fv(gl::GetUniformLocation(obj_shader, s.as_ptr()), 1, gl::FALSE, p1model.as_ptr());
                
                gl::DrawElements(gl::TRIANGLES, 6, gl::UNSIGNED_INT, std::ptr::null());

                let mut p2model = glm::Mat4::identity();
                p2model = glm::translate(&p2model, &player2.0);
                p2model = glm::scale(&p2model, &glm::vec3(0.1, 0.8, 1.0));

                let s = CString::new("model").unwrap();
                gl::UniformMatrix4fv(gl::GetUniformLocation(obj_shader, s.as_ptr()), 1, gl::FALSE, p2model.as_ptr());
                
                gl::DrawElements(gl::TRIANGLES, 6, gl::UNSIGNED_INT, std::ptr::null());

                gl_assert!();
                
                gl::BindVertexArray(cvao);

                let mut ballmodel = glm::Mat4::identity();
                ballpos.0.x += delta as f32 * balldirection.x; 
                ballpos.0.y += delta as f32 * balldirection.y;
                ballmodel = glm::translate(&ballmodel, &ballpos.0);
                ballmodel = glm::scale(&ballmodel, &glm::vec3(0.2, 0.2, 1.0));

                let s = CString::new("model").unwrap();
                gl::UniformMatrix4fv(gl::GetUniformLocation(obj_shader, s.as_ptr()), 1, gl::FALSE, ballmodel.as_ptr());

                gl::DrawArrays(gl::TRIANGLE_FAN, 0, 38);
                gl_assert!();


                let mut linemodel = glm::Mat4::identity();
                linemodel = glm::translate(&linemodel, &glm::vec3(-1.0, 0.95, 0.0));
                linemodel = glm::scale(&linemodel, &glm::vec3(20.0, 1.0, 1.0));
                
                let s = CString::new("model").unwrap();
                gl::UniformMatrix4fv(gl::GetUniformLocation(obj_shader, s.as_ptr()), 1, gl::FALSE, linemodel.as_ptr());
                
                gl::LineWidth(16.0);
                gl::DrawArrays(gl::LINES, 0, 2);
                gl_assert!();

                let mut line2model = glm::Mat4::identity();
                line2model = glm::translate(&line2model, &glm::vec3(-1.0, -0.95, 0.0));
                line2model = glm::scale(&line2model, &glm::vec3(20.0, 1.0, 1.0));
                
                let s = CString::new("model").unwrap();
                gl::UniformMatrix4fv(gl::GetUniformLocation(obj_shader, s.as_ptr()), 1, gl::FALSE, line2model.as_ptr());
                
                gl::DrawArrays(gl::LINES, 0, 2);
                gl_assert!();

            }
        }

        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events){
            match event {
                glfw::WindowEvent::Key(Key::Escape, _, Action::Press, _) => {
                    window.set_should_close(true);
                },
                glfw::WindowEvent::Key(Key::Enter, _, Action::Press, _) => {
                    game_start = true;
                },
                glfw::WindowEvent::Key(Key::W, _, Action::Repeat, _) | 
                glfw::WindowEvent::Key(Key::W, _, Action::Press, _) => {
                    player1.0.y = f32::min(player1.0.y + (3.5 * delta as f32), 0.53f32);
                },
                glfw::WindowEvent::Key(Key::S, _, Action::Repeat, _) |
                glfw::WindowEvent::Key(Key::S, _, Action::Press, _) => {
                    player1.0.y = f32::max(player1.0.y - (3.5 * delta as f32), -0.93f32);
                },
                glfw::WindowEvent::Key(Key::Up, _, Action::Repeat, _) |
                glfw::WindowEvent::Key(Key::Up, _, Action::Press, _) => {
                    player2.0.y = f32::min(player2.0.y + (3.5 * delta as f32), 0.53f32);
                },
                glfw::WindowEvent::Key(Key::Down, _, Action::Repeat, _) |
                glfw::WindowEvent::Key(Key::Down, _, Action::Press, _) => {
                    player2.0.y = f32::max(player2.0.y - (3.5 * delta as f32), -0.93f32);
                },
                _ => ()
            }           
        }
    }
}

fn check_collision(posa: Position, sizea: glm::Vec2, posb: Position, sizeb: glm::Vec2) -> bool {
    let collision_x = posa.0.x + sizea.x >= posb.0.x + sizeb.x &&
        posb.0.x + sizeb.x >= posa.0.x;
    let collision_y = posa.0.y + sizea.y >= posb.0.y + sizeb.y &&
        posb.0.y + sizeb.y >= posa.0.y;
    collision_x && collision_y
}

fn render_text(characters: &HashMap<char, Character>, shader: u32, vao: u32, vbo: u32, text: &str, x: f32, y: f32, rem: f32, color: na::Vector3<f32>){ 
    debug_assert!(match x { 0.0..=1.0 => true, _ => false}, "x out of range");
    debug_assert!(match y { 0.0..=1.0 => true, _ => false}, "y out of range");
    // Normalize coordinates
    let mut x = x * VIEWPORT.lock().unwrap().width() as f32;
    let y = y * VIEWPORT.lock().unwrap().height() as f32;
    // Scale is proportional to 16px;
    let scale = rem / 3.0;
    unsafe {
        gl::UseProgram(shader);

        let id = CString::new("textColor").unwrap();
        gl::Uniform3f(gl::GetUniformLocation(shader, id.as_ptr()),
            color.x, color.y, color.z
        );
        gl_assert!();

        gl::BindVertexArray(vao);
        
        for c in text.chars() {
            let ch = characters.get(&c).unwrap();

            let xpos = x + ch.bearing.x as f32 * scale;
            let ypos = y - (ch.size.y as i32 - ch.bearing.y) as f32 * scale;

            let w = ch.size.x as f32 * scale;
            let h = ch.size.y as f32 * scale;

            let vertices: [f32; 4 * 6] = [
                xpos, ypos + h, 0.0, 0.0,
                xpos, ypos,     0.0, 1.0,
                xpos + w, ypos, 1.0, 1.0,
                
                xpos, ypos + h, 0.0, 0.0,
                xpos + w, ypos, 1.0, 1.0,
                xpos + w, ypos + h, 1.0, 0.0
            ];

            //println!("{:.2?}", vertices);
            
            gl::BindTextureUnit(0, ch.texture_id);

            gl::NamedBufferSubData(vbo, 0, std::mem::size_of_val(&vertices) as _,
                vertices.as_ptr().cast()); 
            
            gl::DrawArrays(gl::TRIANGLES, 0, 6);
            gl_assert!();


            x += ch.advance as f32 * scale;
        }
    }
}

fn gen_font(characters: &mut HashMap<char, Character>) {

    // default size = 16px;
    let font = include_bytes!("/usr/share/fonts/opentype/fira/FiraMono-Regular.otf");

    let settings = fontdue::FontSettings {
        scale: 48.0,
        ..fontdue::FontSettings::default()
    };

    let face = fontdue::Font::from_bytes(font.as_slice(), settings).unwrap();

    for c in '\0'..std::char::from_u32(128).unwrap() {
        
        let (metrics, bitmap) = face.rasterize(c, 48.0);

        let mut texture = 0;
        unsafe {

            gl::CreateTextures(gl::TEXTURE_2D, 1, &mut texture);
            assert_ne!(texture, 0);

            gl::BindTexture(gl::TEXTURE_2D, texture);
            gl::TexImage2D(
                gl::TEXTURE_2D,
                0,
                gl::RED as _,
                metrics.width as _,
                metrics.height as _,
                0,
                gl::RED as _,
                gl::UNSIGNED_BYTE,
                bitmap.as_ptr().cast(),
            );
            gl_assert!();

            gl::TextureParameteri(texture, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as _);
            gl::TextureParameteri(texture, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as _);
            gl::TextureParameteri(texture, gl::TEXTURE_MIN_FILTER, gl::LINEAR as _);
            gl::TextureParameteri(texture, gl::TEXTURE_MAG_FILTER, gl::LINEAR as _);
            
        }

        characters.insert(c, Character {
            texture_id: texture,
            size: na::Vector2::new(metrics.width as i32, metrics.height as i32),
            bearing: na::Vector2::new(metrics.xmin,
                metrics.height as i32 + metrics.ymin),
            advance: metrics.advance_width.ceil() as u32,
        });
    }

}

fn gen_circle(center: glm::Vec2, radius: f32) -> Vec<f32> {
    let mut vertices = Vec::new();
    
    vertices.push(center.x);
    vertices.push(center.y);

    for i in 0..=36 {
        let angle: f32 = 2.0f32 * glm::pi::<f32>() * i as f32 / 36.0;
        let x = center.x + radius * angle.cos();
        let y = center.y + radius * angle.sin();
        vertices.push(x);
        vertices.push(y);
    }

    vertices
}

fn check_shader_error(name: &str, shader: u32) {
    let mut success = 1;
    let mut buf = [0_u8; 512];

    unsafe {
        gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut success);

        if success == 0 {
            gl::GetShaderInfoLog(shader, 512, std::ptr::null_mut(), buf.as_mut_ptr().cast());
            println!("ERROR::{name}: {}", String::from_utf8_lossy(&buf));
        }
    }
}

fn check_program_error(program: u32) {
    let mut success = 1;
    let mut buf = [0_u8; 512];

    unsafe {
        gl::GetProgramiv(program, gl::LINK_STATUS, &mut success);

        if success == 0 {
            gl::GetProgramInfoLog(program, 512, std::ptr::null_mut(), buf.as_mut_ptr().cast());
            println!("ERROR::PROGRAM_SHADER: {}", String::from_utf8_lossy(&buf));
        }
    }

}
