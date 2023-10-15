import { WASI } from "@bjorn3/browser_wasi_shim";
import { Buffer } from 'buffer';

const haskellWasmPath = "./dist/core-game.wasm"

var pressed={};
var mouseX = 0;
var mouseY = 0;

// Add event listeners on keys
document.addEventListener('keydown', (event) => {
    var name = event.key;
    var code = event.code;
    pressed[code] = true;
  }, false);

document.addEventListener('keyup', (event) => {
    var name = event.key;
    var code = event.code;
    delete pressed[code];
  }, false);

// Add event listener on mouse position
document.addEventListener('mousemove', (event) => {
    mouseX = event.clientX;
    mouseY = event.clientY;
})

async function run() {
    const wasi = new WASI([], [], []);
    const wasiImportObj = { wasi_snapshot_preview1: wasi.wasiImport };
    const wasm = await WebAssembly.compileStreaming(fetch(haskellWasmPath));
    const inst = await WebAssembly.instantiate(wasm, wasiImportObj);
    console.log("Calling WASI init function.");
    wasi.initialize(inst);
    inst.exports.hs_init(0, 0);
    console.log("Initialized WASI reactor.");

    const memory = inst.exports.memory;
    const encoder = new TextEncoder();
    const decoder = new TextDecoder();

    // Just an example of sending and receving
    // byte arrays to and from a WASI reactor
    // Followed this example:
    // https://github.com/willmcpherson2/ghc-wasm-experiment/tree/main
    const inputData = "Test String!"
    const inputLen = Buffer.byteLength(inputData);
    const inputPtr = inst.exports.malloc(inputLen);
    const inputArr = new Uint8Array(memory.buffer, inputPtr, inputLen);
    encoder.encodeInto(inputData, inputArr);

    const outputPtr = inst.exports.reverseCharArray(inputPtr, inputLen);
    const outputArr = new Uint8Array(memory.buffer, outputPtr, inputLen);
    const output = decoder.decode(outputArr);
    console.log(`'${inputData}' reversed is '${output}'`)
    inst.exports.free(inputPtr);
    inst.exports.free(outputPtr);



    setInterval(function() {
        // run game step
        inst.exports.runStep(mouseX, mouseY);
        
        // Right now 2 output variables are received using two functions
        // This can be replaces by reading/writing to some byte arrays
        const outX = inst.exports.getOutX();
        const outY = inst.exports.getOutY();
        render(outX, outY);
    }, 10);
}

function render(centerX, centerY) {
    const canvas = document.getElementById('GameCanvas');
    const context = canvas.getContext('2d');
    const radius = 30;
    context.fillStyle="rgb(30,20,100)"
    context.fillRect(0, 0, 1000, 1000);

    context.beginPath();
    context.arc(centerX, centerY, radius, 0, 2 * Math.PI, false);
    context.fillStyle = "rgb(100,200,20)";
    context.fill();
    context.lineWidth = 5;
    context.strokeStyle = '#003300';
    context.stroke();
}

run();


