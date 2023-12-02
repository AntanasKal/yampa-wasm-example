import { WASI } from "@bjorn3/browser_wasi_shim";
import { Buffer } from 'buffer';

const haskellWasmPath = "./dist/game-core.wasm"

const canvas = document.getElementById('GameCanvas');
const context = canvas.getContext('2d');

// TODO: check if they need to be global
var memory = null;
var encoder = null;
var decoder = null;

// Functions that will be called from Haskell
const externalFunctions = {
    renderCircle : (posX, posY, radius, colR, colG, colB) => {
        context.beginPath();
        context.arc(posX, posY, radius, 0, 2 * Math.PI, false);
        context.fillStyle = `rgb(${colR},${colG},${colB})`;
        context.fill();
    },
    clearCanvas : (colR, colG, colB) => {
        context.fillStyle=`rgb(${colR},${colG},${colB})`;
        context.fillRect(0, 0, canvas.width, canvas.height);
    },
    fillStyle : (colR, colG, colB) => {
        context.fillStyle=`rgb(${colR},${colG},${colB})`;
    },
    fillRect : (x, y, width, height) => {
        context.fillRect(x, y, width, height);
    },
    getCanvasWidth : () => {
        return canvas.width;
    },
    getCanvasHeight : () => {
        return canvas.height;
    },
    fillText : (textPtr, textLen, x, y, maxWidth) => {
        console.log(textLen);
        console.log(textPtr);
        context.font = "50px serif";
        const textArr = new Uint8Array(memory.buffer, textPtr, textLen);
        const text = decoder.decode(textArr);
        context.fillText(text, x, y, maxWidth);
    }
}

var pressed = {};
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
    var rect = canvas.getBoundingClientRect();
    mouseX = event.clientX - rect.left;
    mouseY = event.clientY - rect.top;
})

async function run() {
    const wasi = new WASI([], [], []);
    const wasiImportObj = { 
        wasi_snapshot_preview1: wasi.wasiImport,
        env: externalFunctions
    };
    const wasm = await WebAssembly.compileStreaming(fetch(haskellWasmPath));
    const inst = await WebAssembly.instantiate(wasm, wasiImportObj);
    console.log("Calling WASI init function.");
    wasi.initialize(inst);
    inst.exports.hs_init(0, 0);
    console.log("Initialized WASI reactor.");

    memory = inst.exports.memory;
    encoder = new TextEncoder();
    decoder = new TextDecoder();

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
        // run game step and render
        inst.exports.runGameStep(mouseX, mouseY);
    }, 10);
}

run();
