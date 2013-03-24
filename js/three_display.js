window.Three = {};
window.Three.display = (function () {

    function init () {
        var container = $('#rot-container');

        var winW = $(window).width();
        var winH = $(window).height();

        //CAMERA
        var camera = new THREE.PerspectiveCamera(
            20,
            winW / winH,
            1, 1500);

        camera.position.set( 0, 400, 700 );

        var cameraTarget = new THREE.Vector3( 0, 150, 0  );

        //SCENE
        var scene = new THREE.Scene();
        scene.fog = new THREE.Fog( 0x000000, 250, 1400 );

        //LIGHTS
        var dirLight = new THREE.DirectionalLight( 0xffffff, 0.125 );
        dirLight.position.set( 0, 0, 1 );
        scene.add( dirLight );

        var pointLight = new THREE.PointLight( 0xffffff, 1.5 );
        pointLight.position.set( 0, 100, 900 ); //光源
        scene.add( pointLight );

        pointLight.color.setHSL( Math.random(), 1, 0.5 );
        var hex = decimalToHex( pointLight.color.getHex() );

        //MATERIAL
        material = new THREE.MeshFaceMaterial([
            new THREE.MeshPhongMaterial({
                color: 0xffffff, shading: THREE.FlatShading
            }),
            new THREE.MeshPhongMaterial({
                color: 0xffffff, shading: THREE.SmoothShading
            })
        ]);

        var parent = new THREE.Object3D();
        parent.position.y = 100;

        scene.add( parent );


        //PLANE
        var plane = new THREE.Mesh( new THREE.PlaneGeometry( 10000, 10000 ),
                                    new THREE.MeshBasicMaterial({
                                        color: 0xffffff,
                                        opacity: 0.5,
                                        transparent: true
                                    }));
        plane.position.y = 100;
        plane.rotation.x = - Math.PI / 2;
        scene.add( plane );

        //RENDERER
        var renderer = new THREE.WebGLRenderer({ antialias: true });
        renderer.setSize( winW, winH );

        renderer.setClearColor( scene.fog.color, 1 );
        $(renderer.domElement).appendTo(container);

        render(renderer, parent, scene, camera, 0, cameraTarget);

        return {
            renderer: renderer,
            parent: parent,
            scene: scene,
            camera: camera,
            cameraTarget: cameraTarget
        };
    }

    function createText (opt, x, y, z) {
        var textGeo = new THREE.TextGeometry(opt.text, {
            size: opt.size || 10,
            height: opt.height || 10,
            curveSegments: opt.curveSegments || 5,
            font: opt.font || "optimer",
            weight: opt.weight || "bold",
            style: opt.style || "normal"
//            bevelThickness: opt.bevelThickness || 2,
//            bevelSize: opt.bevelSize || 1.5,
//            bevelEnabled: true,
//            extrudeMaterial: 1
        });

        textGeo.computeBoundingBox();
        textGeo.computeVertexNormals();

        var textMesh1 = new THREE.Mesh( textGeo, new THREE.MeshBasicMaterial({
            color: opt.color, overdraw: true
        }));

        textMesh1.position.x = -0.5 * opt.x;
        textMesh1.position.y = opt.y;
        textMesh1.position.z = opt.z;

        textMesh1.rotation.x = 0;
        textMesh1.rotation.y = Math.PI * 2;

        return textMesh1;
    }

    function render (renderer, parent, scene, camera, targetRotation, cameraTarget) {
        parent.rotation.y += (targetRotation - parent.rotation.y) * 0.05;
        camera.lookAt(cameraTarget);
        renderer.clear();
        renderer.render(scene, camera);
    }

    function decimalToHex( d ) {
        var hex = Number( d ).toString( 16 );
        hex = "000000".substr( 0, 6 - hex.length ) + hex;
        return hex.toUpperCase();
    }



    function colorToHex(color) {
        if (color.substr(0, 1) === '#') {
            return color.slice(1);
        }
        var digits = /(.*?)rgb\((\d+),(\d+),(\d+)\)/.exec(color);

        var red = parseInt(digits[2]);
        var green = parseInt(digits[3]);
        var blue = parseInt(digits[4]);

        var rgb = blue | (green << 8) | (red << 16);
        return Number(digits[1] + '0x' + rgb.toString(16));
    };

    window.colorToHex = colorToHex;


    var config;
    //$(function () {
        config = init();
    //});


    return {
        draw: function (x, z, ch, color) {
            x=x-10;
            z=z-10;
            config.parent.add(createText({
                text: ch,
                height: 3, //厚さ
                size: 35,   //大きさ
                color: colorToHex(color),

                x: x*60,     //横 0が真ん中
                y: 10,      //上下 0が真ん中
                z: z*60,     //前後 0が真ん中

                curveSegments: 4,

//                bevelThickness: 2,
//                bevelSize: 1.5,
//                bevelSegments: 3,
//                bevelEnabled: true,

                font: "optimer",
                weight: "bold",
                style: "normal"
            }));
            render(config.renderer,
                   config.parent,
                   config.scene,
                   config.camera,
                   0,
                   config.cameraTarget);
        }
    };

}());
